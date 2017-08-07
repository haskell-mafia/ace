{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ace.Robot.Beaconsfield (
    beaconsfield
  ) where

import qualified Ace.Analysis.Score as Score
import           Ace.Data.Config
import           Ace.Data.Core
import           Ace.Data.Robot

import           Data.Binary (Binary)
import qualified Data.Map as Map

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

beaconsfield :: Robot
beaconsfield =
  Robot "beaconsfield" init move

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

move :: [PunterMove] -> Beaconsfield -> IO (RobotMove Beaconsfield)
move moves state0 =
  let
    state =
      update moves state0

    scores =
      sortOn (Down . snd) . Map.toList $
        Score.choices (beaconsfieldPunter state) (beaconsfieldScoreState state)
  in
    case scores of
      [] ->
        pure $ RobotMove Pass state
      (x, _) : _ ->
        pure $ RobotMove (Claim x) state
