{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Ace.Analysis.Score (
    State(..)
  , init
  , update

  , score
  , scoreJourney

  , choices
  , journeys
  ) where

import qualified Ace.Analysis.River as River
import           Ace.Data.Analysis
import           Ace.Data.Binary ()
import           Ace.Data.Core

import           Data.Binary (Binary)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Unboxed as Unboxed

import           GHC.Generics (Generic)

import           P


data State =
  State {
      statePunters :: !PunterCount
    , stateRiver :: !River.State
    , stateMines :: !(Map MineId (Map SiteId Route))
    , stateJourneys :: !(Map Journey Distance)
    } deriving (Eq, Show, Generic)

instance Binary State

-- FIX lens
withRiverState :: (River.State -> River.State) -> State -> State
withRiverState f state =
  state { stateRiver = f (stateRiver state) }

initMines :: Unboxed.Vector MineId -> River.State -> Map MineId (Map SiteId Route)
initMines mines rivers =
  let
    tree mine =
      (mine, River.routes mine rivers)
  in
    Map.fromList . fmap tree $ Unboxed.toList mines

initJourneys :: Map MineId (Map SiteId Route) -> Map Journey Distance
initJourneys mines =
  Map.fromList .
  concat .
  with (Map.toList mines) $ \(mine, routes) ->
  with (Map.toList routes) $ \(site, route) ->
    (Journey mine site, routeDistance route)

init :: PunterCount -> World -> State
init pcount world =
  let
    rivers =
      River.init world

    mines =
      initMines (worldMines world) rivers

    journeys0 =
      initJourneys mines
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

    takeJourneys mine _routes =
      fmap (Journey mine) $ River.reachable (getMineId mine) rivers
  in
    Map.mapWithKey takeJourneys (stateMines state)

scoreJourney :: Journey -> State -> Score
scoreJourney journey state =
  fromMaybe 0 . fmap distanceScore . Map.lookup journey $ stateJourneys state

scoreJourneys :: Map MineId [Journey] -> State -> Score
scoreJourneys journeys0 state =
  sum . fmap (flip scoreJourney state) . concat $ Map.elems journeys0

score :: PunterId -> State -> Score
score punter state =
  scoreJourneys (journeys punter state) state

choices :: PunterId -> State -> Map River Score
choices punter state =
  let
    unclaimed =
      River.unclaimed $ stateRiver state

    scoreRiver river =
      score punter $ update [PunterMove punter (Claim river)] state
  in
    Map.fromSet scoreRiver unclaimed
