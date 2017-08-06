{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Ace.Data.Web (
    OnlineState(..)
  , GameId(..)
  , indexPage
  , gamesPrefix
  , generateNewId
  ) where

import           Ace.Data.Core

import qualified Data.Text as Text

import           P

import           GHC.Generics (Generic)

import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import           System.IO (IO)
import qualified System.IO as IO
import qualified System.Random as Random

import           Text.Printf (printf)

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

gamesPrefix :: IO.FilePath
gamesPrefix =
  "webcloud/games"

generateNewId :: IO GameId
generateNewId =
  let
    loop n = do
      i <- Random.randomRIO (0, 10000 :: Int)
      exists <- Directory.doesDirectoryExist $ gamesPrefix `FilePath.combine` (show i)
      if not $ exists then
        pure . GameId . Text.pack $ printf "%05d" i
      else if n <= 20 then
        loop (n + 1)
      else
        fail "Could not generate game id"
  in
    loop (0 :: Int)
