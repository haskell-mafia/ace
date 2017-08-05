{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ace.Robot.Pacman (
    pacman
  ) where

import           Ace.Data
import           Ace.Score as Score

import           Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Graph.Inductive.Basic as Graph
import qualified Data.Graph.Inductive.Graph as Graph
import           Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Graph.Inductive.Query.SP as Graph

--import qualified Data.Vector.Unboxed as Unboxed

import           GHC.Generics (Generic)

import           P

import           System.IO (IO)
--import           System.Random (randomRIO)

data PacmanState =
  PacmanState {
      pacmanStateGraph :: Gr SiteId River
    } deriving (Eq, Show, Generic)

instance FromJSON PacmanState where
instance ToJSON PacmanState where

pacman :: Robot PacmanState
pacman =
  Robot "pacman" init move toJSON parseJSON

init :: Setup -> IO (Initialisation PacmanState)
init s =
  pure $ Initialisation (PacmanState . Score.fromWorld $ setupWorld s) []

move :: Gameplay -> State PacmanState -> IO (RobotMove PacmanState)
move _ s =
  pure $ RobotPass (stateData s)
