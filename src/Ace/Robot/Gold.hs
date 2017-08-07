{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ace.Robot.Gold (
    gold
  ) where

import qualified Ace.Analysis.River as River
import qualified Ace.Analysis.Score as Score
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


data Gold =
  Gold {
      goldPunter :: PunterId
    , goldScoreState :: Score.State
    } deriving (Eq, Show, Generic)

instance Binary Gold where

-- FIX lens
updateScoreState :: (Score.State -> Score.State) -> Gold -> Gold
updateScoreState f g =
  g { goldScoreState = f (goldScoreState g) }

gold :: Robot
gold =
  Robot "gold" init move

init :: PunterId -> PunterCount -> World -> Config -> IO (Initialisation Gold)
init punter pcount world _config =
  let
    state =
      Gold punter (Score.init pcount world)

    futures =
      []
  in
    pure $
      Initialisation state futures

update :: [PunterMove] -> Gold -> Gold
update moves state0 =
  updateScoreState (Score.update moves) state0

remaining :: Set River -> Route -> Unboxed.Vector River
remaining unclaimed =
  Unboxed.filter (flip Set.member unclaimed) . routeRivers

move :: [PunterMove] -> Gold -> IO (RobotMove Gold)
move moves state0 =
  let
    state =
      update moves state0

    sstate =
      goldScoreState state

    punter =
      goldPunter state

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
