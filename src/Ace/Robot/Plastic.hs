{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ace.Robot.Plastic (
    plastic
  ) where

import qualified Ace.Analysis.Ledger as River
import qualified Ace.Analysis.Plastic as Score
import           Ace.Data.Analysis
import           Ace.Data.Config
import           Ace.Data.Core
import           Ace.Data.Robot

import           Data.Binary (Binary)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as Unboxed

import           GHC.Generics (Generic)

import           P

import           System.IO (IO)


data Plastic =
  Plastic {
      plasticPunter :: PunterId
    , plasticScoreState :: Score.State
    } deriving (Eq, Show, Generic)

instance Binary Plastic where

-- FIX lens
updateScoreState :: (Score.State -> Score.State) -> Plastic -> Plastic
updateScoreState f g =
  g { plasticScoreState = f (plasticScoreState g) }

plastic :: Robot
plastic =
  Robot "plastic" init move

init :: PunterId -> PunterCount -> World -> Config -> IO (Initialisation Plastic)
init punter pcount world _config =
  let
    state =
      Plastic punter (Score.init pcount world)

    futures =
      []
  in
    pure $
      Initialisation state futures

update :: [PunterMove] -> Plastic -> Plastic
update moves state0 =
  updateScoreState (Score.update moves) state0

remaining :: Set River -> Route -> Unboxed.Vector River
remaining unclaimed =
  Unboxed.filter (flip Set.member unclaimed) . routeRivers

move :: [PunterMove] -> Plastic -> IO (RobotMove Plastic)
move moves state0 =
  let
    state =
      update moves state0

    sstate =
      plasticScoreState state

    punter =
      plasticPunter state

    routes =
      Score.routes punter sstate

    unclaimed =
      River.unclaimed (Score.stateRiver sstate)

    journeyRemaining :: Map Journey (Unboxed.Vector River)
    journeyRemaining =
      fmap (remaining unclaimed) routes

    _slow :: Map Journey (Score, Unboxed.Vector River)
    _slow =
      with journeyRemaining $ \rs ->
        (Score.scoreClaims punter (Unboxed.toList rs) sstate, rs)

    _quick :: Map Journey (Score, Unboxed.Vector River)
    _quick =
      Map.intersectionWith
        (\d rs -> (distanceScore d, rs))
        (Score.stateJourneys sstate)
        journeyRemaining

    best :: [(Score, Unboxed.Vector River)]
    best =
      sortOn (\(s, rs) -> (Down s, Unboxed.length rs)) $
      Map.elems _slow

    decision =
      Just . maybe Pass Claim . head $ mapMaybe ((Unboxed.!? 0) . snd) best
  in
    pure $ RobotMove decision state
