{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ace.Robot.Carpe (
    carpe
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
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as Unboxed

import           GHC.Generics (Generic)

import           P

import           System.IO (IO)


data Carpe =
  Carpe {
      carpePunter :: PunterId
    , carpePunterCount :: PunterCount
    , carpeOptions :: Int
    , carpeScoreState :: Score.State
    } deriving (Eq, Show, Generic)

instance Binary Carpe where

-- FIX lens
updateScoreState :: (Score.State -> Score.State) -> Carpe -> Carpe
updateScoreState f g =
  g { carpeScoreState = f (carpeScoreState g) }

carpe :: Robot
carpe =
  Robot "carpe" init move

init :: PunterId -> PunterCount -> World -> Config -> IO (Initialisation Carpe)
init punter pcount world _config =
  let
    state =
      Carpe punter pcount (Unboxed.length $ worldMines world) (Score.init pcount world)

    futures =
      []
  in
    pure $
      Initialisation state futures

update :: [PunterMove] -> Carpe -> Carpe
update moves state0 =
  updateScoreState (Score.update moves) state0

filterMineMine :: Set MineId -> Map Journey a -> Map Journey a
filterMineMine mines =
  Map.filterWithKey $ \x _ ->
    Set.member (MineId (journeyTarget x)) mines

mineMove :: PunterId -> PunterCount -> Map Journey Distance -> River.State -> Set MineId -> Maybe Move
mineMove punter _pcount _journeys rivers mines =
  let
    owners =
      River.owners rivers

    options =
      Map.filter (River.canOption punter) owners

    current_rivers =
      River.filterPunterOrUnclaimed punter rivers

    current_routes =
      filterMineMine mines $
      River.routes mines current_rivers

    possible =
      sortOn (Down . snd) .
      List.filter ((/= 0) . snd) .
      with (Map.keys options) $ \river ->
        let
          option_rivers =
            River.filterPunterOrUnclaimed punter $
            River.claim (PunterBid BidOption punter river) rivers

          option_routes =
            filterMineMine mines $
            River.routes mines option_rivers

          new_routes =
            option_routes `Map.difference` current_routes
        in
          (river, Map.size new_routes)
  in
    case head possible of
      Nothing ->
        Nothing
      Just (river, _score) ->
        Just $ Option river

move :: [PunterMove] -> Carpe -> IO (RobotMove Carpe)
move moves state0 =
  let
    state =
      update moves state0

    stateUsedOption =
      state { carpeOptions = carpeOptions state - 1 }

    punter =
      carpePunter state

    pcount =
      carpePunterCount state

    sstate =
      carpeScoreState state

    rivers =
      Score.stateRiver sstate

    mines =
      Score.stateMines sstate

    journeys =
      Score.stateJourneys sstate

    moveOrGiveUp =
      mineMove punter pcount journeys rivers mines
  in
    if carpeOptions state0 == 0 then
      pure $ RobotMove Nothing state0
    else
      case moveOrGiveUp of
        Nothing ->
          pure $ RobotMove Nothing state
        Just m ->
          pure $ RobotMove (Just m) stateUsedOption
