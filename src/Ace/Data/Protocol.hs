{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Ace.Data.Protocol (
    Punter(..)
  , PunterId(..)
  , PunterCount(..)
  , Move(..)
  , OfflineRequest(..)
  , Setup(..)
  , Stop(..)
  , MovesOrStop(..)
  , Gameplay(..)
  , PunterScore(..)
  , State(..)
  , MoveResult(..)
  , MoveRequestServer(..)
  , SetupResult(..)
  , didIWin
  ) where


import           Ace.Data.Config
import           Ace.Data.Core
import           Ace.Data.Future

import           Data.Aeson (Value)

import           P

import           GHC.Generics (Generic)

import           X.Text.Show (gshowsPrec)


newtype Punter =
  Punter {
      renderPunter :: Text
    } deriving (Eq, Ord, Generic)

instance Show Punter where
  showsPrec =
    gshowsPrec


data OfflineRequest =
    OfflineSetup !Setup
  | OfflineGameplay !Gameplay !State
  | OfflineScoring !Stop !State
    deriving (Eq, Show, Generic)

data Setup =
  Setup {
      setupPunter :: !PunterId
    , setupPunterCount :: !PunterCount
    , setupWorld :: !World
    , setupConfig :: !Config
    } deriving (Eq, Show, Generic)

data Stop =
  Stop {
      stopMoves :: ![PunterMove]
    , stopScores :: ![PunterScore]
    , stopState :: !(Maybe State)
    } deriving (Eq, Show, Generic)

data MovesOrStop =
    JustMoves ![PunterMove]
  | JustStop !Stop
    deriving (Eq, Show, Generic)

newtype Gameplay =
  Gameplay {
      gameplay :: [PunterMove]
    } deriving (Eq, Ord, Show, Generic)

data PunterScore =
  PunterScore {
      scorePunter :: !PunterId
    , scoreValue :: !Score
    } deriving (Eq, Ord, Generic)


instance Show PunterScore where
  showsPrec =
    gshowsPrec

data State =
  State {
      statePunter :: !PunterId
    , stateRobot :: !Value
    } deriving (Eq, Show, Generic)

data MoveResult =
  MoveResult {
      moveResultMove :: !PunterMove
    , moveResultState :: !State
    } deriving (Eq, Show, Generic)

data MoveRequestServer =
  MoveRequestServer {
      moveRequestServerMoves :: ![PunterMove]
    , moveRequestServerState :: !State
    } deriving (Eq, Show, Generic)

data SetupResult =
  SetupResult {
      setupResultPunter :: !PunterId
    , setupResultFutures :: ![Future]
    , setupResultState :: !State
    } deriving (Eq, Show, Generic)

didIWin :: PunterId -> Stop -> Bool
didIWin p s =
  fmap scorePunter (head $ sortOn (Down . scoreValue) (stopScores s)) == Just p
