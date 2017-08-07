{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Ace.Analysis.Score (
    State(..)
  , init
  , update

  , score
  , scoreJourney
  , scoreClaim
  , scoreClaims

  , choices
  , routes
  , journeys
  ) where

import qualified Ace.Analysis.River as River
import           Ace.Data.Analysis
import           Ace.Data.Binary ()
import           Ace.Data.Core

import           Data.Binary (Binary)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as Unboxed

import           GHC.Generics (Generic)

import           P


data State =
  State {
      statePunters :: !PunterCount
    , stateRiver :: !River.State
    , stateMines :: !(Set MineId)
    , stateJourneys :: !(Map Journey Distance)
    } deriving (Eq, Show, Generic)

instance Binary State

-- FIX lens
withRiverState :: (River.State -> River.State) -> State -> State
withRiverState f state =
  state { stateRiver = f (stateRiver state) }

init :: PunterCount -> World -> State
init pcount world =
  let
    rivers =
      River.init world

    mines =
      Set.fromList . Unboxed.toList $ worldMines world

    journeys0 =
      fmap routeDistance (River.routes mines rivers)
  in
    State pcount rivers mines journeys0

update :: [PunterMove] -> State -> State
update moves =
  withRiverState (River.update moves)

journeys :: PunterId -> State -> Map MineId [Journey]
journeys punter state =
  let
    rivers =
      River.filterPunter punter (stateRiver state)

    takeJourneys mine =
      fmap (Journey mine) $ River.reachable (getMineId mine) rivers
  in
    Map.fromSet takeJourneys (stateMines state)

scoreJourney :: Journey -> State -> Score
scoreJourney journey state =
  fromMaybe 0 . fmap distanceScore . Map.lookup journey $ stateJourneys state

scoreJourneys :: Map MineId [Journey] -> State -> Score
scoreJourneys journeys0 state =
  sum . fmap (flip scoreJourney state) . concat $ Map.elems journeys0

score :: PunterId -> State -> Score
score punter state =
  scoreJourneys (journeys punter state) state

scoreClaims :: PunterId -> [River] -> State -> Score
scoreClaims punter rivers state =
  score punter $ update (fmap (PunterMove punter . Claim) rivers) state

scoreClaim :: PunterId -> River -> State -> Score
scoreClaim punter river state =
  scoreClaims punter [river] state

-- | Get all the rivers that a punter could claim on their next move, and the
--   score it would yield.
--
choices :: PunterId -> State -> Map River Score
choices punter state =
  let
    unclaimed =
      River.unclaimed $ stateRiver state
  in
    Map.fromSet (\r -> scoreClaim punter r state) unclaimed

-- | Get the shortest available routes for each journey.
--
routes :: PunterId -> State -> Map Journey Route
routes punter state =
  River.routes (stateMines state) $
  River.filterPunterOrUnclaimed punter (stateRiver state)
