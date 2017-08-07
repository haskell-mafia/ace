{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Ace.Data.Web (
    WebPlayer(..)
  , toWebPlayer
  , onlineWebPlayers

  , OnlineState(..)
  , GameId(..)
  , indexPage
  , gamesPrefix
  ) where

import           Ace.Data.Core
import           Ace.Data.Offline
import           Ace.Data.Protocol
import           Ace.Data.Robot

import qualified Data.Text as Text

import           P

import           GHC.Generics (Generic)

import qualified System.IO as IO

import           X.Text.Show (gshowsPrec)

data WebPlayer =
  WebPlayer {
      webPlayerRobot :: !RobotIdentifier
    , webPlayerId :: !PunterId
    } deriving (Eq, Show)

toWebPlayer :: Player -> WebPlayer
toWebPlayer p =
  WebPlayer (playerRobot p) (playerId p)

onlineWebPlayers :: Robot -> PunterId -> PunterCount -> [WebPlayer]
onlineWebPlayers robot me ct =
  with (PunterId <$> [0 .. punterCount ct]) $ \i ->
    if i == me then
      WebPlayer (RobotIdentifier (nameOf robot) (Punter . robotName $ nameOf robot)) me
    else
      WebPlayer (RobotIdentifier (RobotName "online") (Punter "online")) i

data OnlineState =
  OnlineState {
      onlineWorld :: !World
    , onlinePunter :: !PunterId
    } deriving (Eq, Generic)

instance Show OnlineState where
  showsPrec =
    gshowsPrec

newtype GameId =
  GameId {
      gameId :: Text
    } deriving (Eq, Generic)

instance Show GameId where
  showsPrec =
    gshowsPrec

indexPage :: GameId -> Text
indexPage g =
  Text.unlines [
      "<!doctype html>"
    , "<html>"
    , "  <head>"
    , "    <meta http-equiv=\"refresh\" content=\"0; url=../../index.html?game=games/" <> gameId g <> "\" />"
    , "  </head>"
    , "</html>"
    ]

gamesPrefix :: IO.FilePath
gamesPrefix =
  "webcloud/games"
