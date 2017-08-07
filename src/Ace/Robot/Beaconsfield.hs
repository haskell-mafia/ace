{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ace.Robot.Beaconsfield (
    beaconsfield
  ) where

import qualified Ace.Analysis.River as River
import qualified Ace.Analysis.Score as Score
import           Ace.Data.Config
import           Ace.Data.Core
import           Ace.Data.Robot

import           Data.Binary (Binary)
import qualified Data.Binary as Binary
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as Unboxed

import           GHC.Generics (Generic)

import           P

import           System.IO (IO)


data Beaconsfield =
  Beaconsfield {
      beaconsfieldPunter :: PunterId
    , beaconsfieldScoreState :: Score.State
    , beaconsfieldDelegateState :: ByteString
    } deriving (Eq, Show, Generic)

instance Binary Beaconsfield where

-- FIX lens
updateScoreState :: (Score.State -> Score.State) -> Beaconsfield -> Beaconsfield
updateScoreState f g =
  g { beaconsfieldScoreState = f (beaconsfieldScoreState g) }


-- cutoff: when to pick beacosfield over gold
-- bootstrap: number of moves to start caring
beaconsfield :: Double -> Int -> Robot -> Text -> Robot
beaconsfield cutoff bootstrap delegate label =
  Robot label (init delegate) (move cutoff bootstrap delegate)

init :: Robot -> PunterId -> PunterCount -> World -> Config -> IO (Initialisation Beaconsfield)
init delegate punter pcount world config = do
  (dstate, dfutures) <- case delegate of
    Robot _ initx _ -> do
      initialisation <- initx punter pcount world config
      pure $ (Lazy.toStrict . Binary.encode . initialisationState $ initialisation, initialisationFutures initialisation)

  let
    state =
      Beaconsfield punter (Score.init pcount world) dstate

    futures =
      [] <> dfutures

  pure $ Initialisation state futures

update :: [PunterMove] -> ByteString -> Beaconsfield -> Beaconsfield
update moves delegate state0 =
  (updateScoreState (Score.update moves) state0) {
      beaconsfieldDelegateState = delegate
    }

move :: Double -> Int -> Robot -> [PunterMove] -> Beaconsfield -> IO (RobotMove Beaconsfield)
move cutoff bootstrap delegate moves state0 = do
  (dmove, dstate) <- case delegate of
    Robot _ _ movex -> do
      fallback <- movex moves . Binary.decode . Lazy.fromStrict . beaconsfieldDelegateState $ state0
      pure $ (robotMoveValue fallback, Lazy.toStrict . Binary.encode . robotMoveState $ fallback)

  let
    state =
      update moves dstate state0

    score =
      beaconsfieldScoreState state

    river =
      Score.stateRiver score

    punter =
      beaconsfieldPunter state

    owned =
      River.filterPunter punter river

    available =
      River.filterPunterOrUnclaimed punter river

    claimable =
      River.unclaimed river

    current :: Map MineId (Unboxed.Vector SiteId)
    current =
      Map.fromList $ with (Set.toList (Score.stateMines score)) $ \mine ->
        (mine, Unboxed.fromList . fmap fst . Map.toList $ River.routesFor mine owned)


    -- FIX this doesn't look right,
    connections :: Map MineId [(SiteId, Route)]
    connections =
      flip Map.mapWithKey current $ \mine sites ->
        let
          potentials :: [Unboxed.Vector SiteId]
          potentials = fmap (\(m, ss) -> Unboxed.cons (getMineId m) ss) . List.filter ((/=) mine . fst) . Map.toList $ current
        in
          join . with (Unboxed.toList sites) $ \site ->
            let
              cross = River.routesFor (MineId site) available
            in
              join . with potentials $ \potential ->
                join . with (Unboxed.toList potential) $ \one ->
                 case Map.lookup one cross of
                    Nothing ->
                      []
                    Just r ->
                      [(site, r)]

    start :: Score
    start =
      Score.score punter score

    ranked :: [(Route, Score)]
    ranked =
      with (join . Map.elems . (fmap . fmap) snd $ connections) $ \r ->
        (r, Score.score punter $
          Score.update (with (Unboxed.toList $ routeRivers r) $ \v -> PunterMove punter (Claim v)) score)

    -- reward / effort ratio
    ratio :: [(Route, Double)]
    ratio =
      catMaybes . with ranked $ \(r, s) ->
        let
          size = (Unboxed.length . Unboxed.filter (\rr -> Set.member rr claimable) . routeRivers) r
        in
          if size <= 0 then
            Nothing
          else
            Just $ (r, ((fromIntegral (getScore s - getScore start))) / fromIntegral size)

    best :: Maybe (Route, Double)
    best =
      head .
      sortOn (Down . snd) $
      ratio

    candidate :: Maybe (River, Double)
    candidate =
      best >>= \(r, d) -> (fmap (\x -> (x, d)) . head . Unboxed.toList . Unboxed.filter (\rr -> Set.member rr claimable) . routeRivers) r

    winner =
      case candidate of
        Nothing ->
          RobotMove dmove state
        Just (r, d) ->
          if d < cutoff || (length . join . fmap Unboxed.toList . Map.elems $ current) < bootstrap then
            RobotMove dmove state
          else
            RobotMove (Just $ Claim r) state

  pure $ winner
