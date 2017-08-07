{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Ace.Data.Offline (
    Player(..)
  , Result(..)
  , PunterResult(..)
  , punterResults
  ) where

import           Ace.Data.Core
import           Ace.Data.Protocol
import           Ace.Data.Robot

import           GHC.Generics (Generic)

import           P

import qualified System.IO as IO

import           X.Text.Show (gshowsPrec)

data Player =
  Player {
      playerExecutable :: !IO.FilePath
    , playerRobot :: !RobotName
    , playerId :: !PunterId
    , playerState :: !State
    , playerSplurgeBudget :: Int
    } deriving (Eq, Show)


data PunterResult =
  PunterResult {
      punterResultPunter :: !PunterId
    , punterResultRobot :: !RobotName
    , punterResultValue :: !Score
    } deriving (Eq, Ord, Generic)

instance Show PunterResult where
  showsPrec =
    gshowsPrec

punterResults :: [Player] -> [PunterScore] -> [PunterResult]
punterResults _ _ =
  []

data Result =
  Result {
      resultRobot :: !RobotName
    , resultMap :: !Text
    , resultGames :: !Int
    , resultWins :: !Int
    } deriving (Eq, Ord, Generic)

instance Show Result where
  showsPrec =
    gshowsPrec
