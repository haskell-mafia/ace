{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ace.Robot.Gold (
    gold
  ) where

import qualified Ace.Analysis.Score as Score
import           Ace.Data.Config
import           Ace.Data.Core
import           Ace.Data.Robot

import           Data.Binary (Binary)

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

move :: [PunterMove] -> Gold -> IO (RobotMove Gold)
move moves state0 =
  let
    state =
      update moves state0
  in
    pure $ RobotMove Pass state
