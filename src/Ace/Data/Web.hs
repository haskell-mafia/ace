{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ace.Data.Web (
    OnlineState(..)
  , GameId(..)
  , indexPage
  ) where

import           Ace.Data.Core

import qualified Data.Text as Text

import           P

import           GHC.Generics (Generic)

import           X.Text.Show (gshowsPrec)

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
    , "    <meta http-equiv=\"refresh\" content=\"0; url=http://localhost:8000/webcloud/index.html?game=games/" <> gameId g <> "\" />"
    , "  </head>"
    , "</html>"
    ]
