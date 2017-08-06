{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Ace.Data.Offline (
    Player(..)
  ) where

import           Ace.Data.Core
import           Ace.Data.Protocol
import           Ace.Data.Robot

import           P

import qualified System.IO as IO

data Player =
  Player {
      playerExecutable :: !IO.FilePath
    , playerRobot :: !RobotName
    , playerId :: !PunterId
    , playerState :: !State
    } deriving (Eq, Show)
