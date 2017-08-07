{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ace.Data.Offline (
    Player(..)
  , Result(..)
  , ResultDetail(..)
  , PunterResult(..)
  , punterResults

  , ServerConfig(..)
  ) where

import           Ace.Data.Core
import           Ace.Data.Config
import           Ace.Data.Protocol
import           Ace.Data.Robot

import           GHC.Generics (Generic)

import           P

import qualified System.IO as IO

import           X.Text.Show (gshowsPrec)

data Player =
  Player {
      playerExecutable :: !IO.FilePath
    , playerRobot :: !RobotIdentifier
    , playerId :: !PunterId
    , playerState :: !State
    , playerSplurgeBudget :: Int
    } deriving (Eq, Show)


data PunterResult =
  PunterResult {
      punterResultPunter :: !PunterId
    , punterResultRobot :: !RobotIdentifier
    , punterResultValue :: !Score
    } deriving (Eq, Ord, Generic)

instance Show PunterResult where
  showsPrec =
    gshowsPrec

punterResults :: [Player] -> [PunterScore] -> [PunterResult]
punterResults players scores =
  with scores $ \(PunterScore p s) ->
    case find (\x -> p == playerId x) players of
      Nothing ->
        PunterResult p (RobotIdentifier (RobotName "unknown") (Punter "unknown")) s
      Just n ->
        PunterResult p (playerRobot n) s

data Result =
  Result {
      resultRobot :: !RobotName
    , resultPunter :: !Punter
    , resultMap :: !Text
    , resultDetail :: !ResultDetail
    } deriving (Eq, Ord, Generic)

instance Show Result where
  showsPrec =
    gshowsPrec

data ResultDetail =
  ResultDetail {
      resultDetailGames :: !Int
    , resultDetailWins :: !Int
    } deriving (Eq, Ord, Generic)

instance Monoid ResultDetail where
  (ResultDetail x0 y0) `mappend` (ResultDetail x1 y1) =
    ResultDetail (x0 + x1) (y0 + y1)

  mempty = ResultDetail 0 0

instance Show ResultDetail where
  showsPrec =
    gshowsPrec


data ServerConfig =
  ServerConfig {
      serverWorldConfig :: !Config
    , serverLogWeb :: !Bool
    } deriving (Eq, Show, Generic)
