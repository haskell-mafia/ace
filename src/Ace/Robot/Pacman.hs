{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ace.Robot.Pacman (
    pacman
  ) where

import           Ace.Data

import           Data.Aeson (FromJSON (..), ToJSON (..))
--import qualified Data.Vector.Unboxed as Unboxed

import           GHC.Generics (Generic)

import           P

import           System.IO (IO)
--import           System.Random (randomRIO)

data PacmanState =
  PacmanState
  deriving (Eq, Show, Generic)

instance FromJSON PacmanState where
instance ToJSON PacmanState where

pacman :: Robot PacmanState
pacman =
  Robot "pacman" init move toJSON parseJSON

init :: Setup -> IO (Initialisation PacmanState)
init _ =
  pure $ Initialisation PacmanState []

move :: Gameplay -> State PacmanState -> IO (RobotMove PacmanState)
move _ s =
  pure $ RobotPass (stateData s)
