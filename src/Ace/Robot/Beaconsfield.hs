{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ace.Robot.Beaconsfield (
    beaconsfield
  ) where

import qualified Ace.Analysis.River as River
import qualified Ace.Analysis.Score as Score
import           Ace.Data.Analysis
import           Ace.Data.Config
import           Ace.Data.Core
import           Ace.Data.Robot

import           Data.Binary (Binary)
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as Unboxed

import           GHC.Generics (Generic)

import           P

import           System.IO (IO)


data Beaconsfield =
  Beaconsfield {
      beaconsfieldPunter :: PunterId
    , beaconsfieldScoreState :: Score.State
    } deriving (Eq, Show, Generic)

instance Binary Beaconsfield where

-- FIX lens
updateScoreState :: (Score.State -> Score.State) -> Beaconsfield -> Beaconsfield
updateScoreState f g =
  g { beaconsfieldScoreState = f (beaconsfieldScoreState g) }

-- cutoff: when to pick beacosfield over gold
-- bootstrap: number of moves to start caring
beaconsfield :: Double -> Int -> Robot
beaconsfield cutoff bootstrap =
  Robot "beaconsfield" init (move cutoff bootstrap)

init :: PunterId -> PunterCount -> World -> Config -> IO (Initialisation Beaconsfield)
init punter pcount world _config =
  let
    state =
      Beaconsfield punter (Score.init pcount world)

    futures =
      []
  in
    pure $
      Initialisation state futures

update :: [PunterMove] -> Beaconsfield -> Beaconsfield
update moves state0 =
  updateScoreState (Score.update moves) state0

move :: Double -> Int -> [PunterMove] -> Beaconsfield -> IO (RobotMove Beaconsfield)
move cutoff bootstrap moves state0 =
  let
    state =
      update moves state0

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

    connections :: Map MineId (Map SiteId Route)
    connections =
      flip Map.mapWithKey current $ \mine sites ->
        let
          potentials :: [Unboxed.Vector SiteId]
          potentials = fmap (\(m, ss) -> Unboxed.cons (getMineId m) ss) . List.filter ((/=) mine . fst) . Map.toList $ current
        in
          Map.fromList . join . with (Unboxed.toList sites) $ \site ->
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
      with ((join . fmap Map.elems . Map.elems) connections) $ \r ->
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

    ---- vvv GOLD

    routes =
      Score.routes punter score

    unclaimed =
      River.unclaimed (Score.stateRiver score)

    journeyRemaining :: Map Journey (Unboxed.Vector River)
    journeyRemaining =
      fmap (remaining unclaimed) routes

    _slow :: Map Journey (Score, Unboxed.Vector River)
    _slow =
      with journeyRemaining $ \rs ->
        (Score.scoreClaims punter (Unboxed.toList rs) score, rs)

    _quick :: Map Journey (Score, Unboxed.Vector River)
    _quick =
      Map.intersectionWith
        (\d rs -> (distanceScore d, rs))
        (Score.stateJourneys score)
        journeyRemaining

    bestx :: [(Score, Unboxed.Vector River)]
    bestx =
      sortOn (\(s, rs) -> (Down s, Unboxed.length rs)) $
      Map.elems _slow

    decision =
      maybe Pass Claim . head $ mapMaybe ((Unboxed.!? 0) . snd) bestx


    ---- GOLD ^^^

    winner =
      case candidate of
        Nothing ->
          RobotMove decision state
        Just (r, d) ->
          if d < cutoff || (length . join . fmap Unboxed.toList . Map.elems $ current) < bootstrap then
            RobotMove decision state
          else
            RobotMove (Claim r) state
  in
    pure $ winner

remaining :: Set River -> Route -> Unboxed.Vector River
remaining unclaimed =
  Unboxed.filter (flip Set.member unclaimed) . routeRivers
