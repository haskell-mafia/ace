{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Ace.Data (
    SiteId(..)
  , River(..)
  , World(..)
  , Punter(..)
  , PunterId(..)
  , PunterCount(..)
  , Move(..)
  , Source(..)
  , Target(..)
  , Setup(..)
  , Gameplay(..)
  , Score(..)
  , Scoring(..)
  , State(..)
  , renderSite
  , renderRiver
  , renderWorld
  , renderPunterId
  , renderPunterCount
  , renderWorldAsJson
  , dumpAsJson
  ) where

import           System.IO
import qualified Data.Text.IO as Text

import           P

import qualified Data.Text as Text

import qualified Data.Vector.Unboxed as Unboxed
import           Data.Vector.Unboxed.Deriving (derivingUnbox)


newtype SiteId =
  SiteId {
      siteId :: Int
    } deriving (Eq, Ord, Show)

derivingUnbox "SiteId"
  [t| SiteId -> Int |]
  [| siteId |]
  [| SiteId |]

data River =
  River {
      riverSource :: !SiteId
    , riverTarget :: !SiteId
    } deriving (Eq, Ord, Show)

derivingUnbox "River"
  [t| River -> (SiteId, SiteId) |]
  [| \(River x y) -> (x, y) |]
  [| \(x, y) -> River x y |]

data World =
  World {
      worldSites :: !(Unboxed.Vector SiteId)
    , worldMines :: !(Unboxed.Vector SiteId)
    , worldRivers :: !(Unboxed.Vector River)
    } deriving (Eq, Ord, Show)

newtype Punter =
  Punter {
      renderPunter :: Text
    } deriving (Eq, Ord, Show)

newtype PunterId =
  PunterId {
      punterId :: Int
    } deriving (Eq, Ord, Show)

newtype PunterCount =
  PunterCount {
      punterCount :: Int
    } deriving (Eq, Ord, Show)

data Move =
    Claim !PunterId !Source !Target
  | Pass !PunterId
    deriving (Eq, Ord, Show)

newtype Source =
  Source {
      source :: SiteId
    } deriving (Eq, Ord, Show)

newtype Target =
  Target {
      target :: SiteId
    } deriving (Eq, Ord, Show)

data OfflineRequest =
    OfflineSetup !Setup
  | OfflineGameplay !Gameplay !State
  | OfflineScoring !Scoring !State
    deriving (Eq, Ord, Show)

data Setup =
  Setup {
      setupPunter :: !PunterId
    , setupPunterCounter :: !PunterCount
    , setupWord :: !World
    } deriving (Eq, Ord, Show)

newtype Gameplay =
  Gameplay {
      gameplay :: [Move]
    } deriving (Eq, Ord, Show)

data Score =
  Score {
      scorePunter :: !PunterId
    , scoreValue :: !Int
    } deriving (Eq, Ord, Show)

data Scoring =
  Scoring {
      finalGameplay :: [Move]
    , scores :: [Score]
    } deriving (Eq, Ord, Show)

data State =
    State
    deriving (Eq, Ord, Show)

renderSiteId :: SiteId -> Text
renderSiteId =
  Text.pack . show . siteId

renderSite :: SiteId -> Text
renderSite x =
  "{ \"id\": " <> renderSiteId x <> " }"

renderRiver :: River -> Text
renderRiver (River x y) =
  "{ \"source\": " <> renderSiteId x  <> ", \"target\": " <> renderSiteId y <> " }"

renderWorld :: World -> [Text]
renderWorld world =
   fmap renderRiver .
   Unboxed.toList .
   worldRivers $ world

renderPunterId :: PunterId -> Text
renderPunterId =
  Text.pack . show . punterId

renderPunterCount :: PunterCount -> Text
renderPunterCount =
  Text.pack . show . punterCount

renderWorldAsJson :: World -> Text
renderWorldAsJson w =
  let
    listOf f x =
      "[" <> Text.intercalate "," (fmap f x) <> "]"

    rivers =
      Unboxed.toList . worldRivers $ w

    mines =
      Unboxed.toList . worldMines $ w

    sites =
      Unboxed.toList . worldSites $ w

  in
    Text.intercalate "\n" $
      [ "var world = {"
      , "\"sites\": " <> listOf renderSite sites <> ","
      , "\"rivers\": " <> listOf renderRiver rivers <> ","
      , "\"mines\": " <> listOf renderSiteId mines
      , "};"
      ]

dumpAsJson :: World -> IO ()
dumpAsJson =
  Text.writeFile "webcloud/world.js" . renderWorldAsJson
