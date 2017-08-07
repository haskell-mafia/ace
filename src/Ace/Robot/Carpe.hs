{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ace.Robot.Carpe (
    carpe
  ) where

import qualified Ace.Analysis.River as River
import qualified Ace.Analysis.Score as Score
import           Ace.Data.Config
import           Ace.Data.Core
import           Ace.Data.Robot

import           Data.Binary (Binary)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           GHC.Generics (Generic)

import           P

import           System.IO (IO)


data Carpe =
  Carpe {
      carpePunter :: PunterId
    , carpePunterCount :: PunterCount
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
      Carpe punter pcount (Score.init pcount world)

    futures =
      []
  in
    pure $
      Initialisation state futures

update :: [PunterMove] -> Carpe -> Carpe
update moves state0 =
  updateScoreState (Score.update moves) state0

mineMove :: PunterId -> PunterCount -> River.State -> MineId -> Maybe Move
mineMove punter _pcount rivers mine =
  let
    threshold = 2
      -- if the other punters take the free slots we're screwed so we better act
--      punterCount pcount - 1

    surrounds =
      River.surrounds (getMineId mine) rivers

    claimed =
      Map.filter isJust surrounds

    unclaimed =
      Map.filter isNothing surrounds

    ours =
      Map.filter (== Just punter) claimed
  in
    if Map.null ours && Map.size unclaimed <= threshold then
      fmap (Claim . makeRiver (getMineId mine)) . head $ Map.keys unclaimed
    else
      Nothing

move :: [PunterMove] -> Carpe -> IO (RobotMove Carpe)
move moves state0 =
  let
    state =
      update moves state0

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

    moveOrGiveUp =
      asum $ fmap (mineMove punter pcount rivers) (Set.toList mines)
  in
    pure $ RobotMove moveOrGiveUp state
