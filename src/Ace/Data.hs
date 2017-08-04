{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Ace.Data (
    SiteId(..)
  , River
  , riverSource
  , riverTarget
  , makeRiver
  , World(..)
  , Punter(..)
  , PunterId(..)
  , PunterCount(..)
  , Move(..)
  , OfflineRequest(..)
  , Setup(..)
  , Stop(..)
  , MovesOrStop(..)
  , Gameplay(..)
  , Score(..)
  , State(..)
  , MoveResult(..)
  , SetupResult(..)
  , Hostname(..)
  , Port(..)

  , Robot(..)
  , RobotMove(..)
  , fromRobotMove

  , renderSite
  , renderRiver
  , renderWorld
  , renderPunterId
  , renderPunterCount
  , renderWorldAsJson
  , dumpAsJson
  ) where

import           P

import           Data.Aeson.Types (Value, Parser)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Vector.Unboxed as Unboxed
import           Data.Vector.Unboxed.Deriving (derivingUnbox)

import           System.IO (IO)


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

makeRiver :: SiteId -> SiteId -> River
makeRiver a b =
  River (min a b) (max a b)

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
    Claim !PunterId !River
  | Pass !PunterId
    deriving (Eq, Ord, Show)

data OfflineRequest a =
    OfflineSetup !Setup
  | OfflineGameplay !Gameplay !(State a)
  | OfflineScoring !Stop !(State a)
    deriving (Eq, Ord, Show)

data Setup =
  Setup {
      setupPunter :: !PunterId
    , setupPunterCount :: !PunterCount
    , setupWorld :: !World
    } deriving (Eq, Ord, Show)

data Stop =
  Stop {
      stopMoves :: ![Move]
    , stopScores :: ![Score]
    } deriving (Eq, Ord, Show)

data MovesOrStop =
    JustMoves ![Move]
  | JustStop !Stop
    deriving (Eq, Show)

newtype Gameplay =
  Gameplay {
      gameplay :: [Move]
    } deriving (Eq, Ord, Show)

data Score =
  Score {
      scorePunter :: !PunterId
    , scoreValue :: !Int
    } deriving (Eq, Ord, Show)

data State a =
    State {
      statePunter :: !PunterId
    , statePunterCount :: !PunterCount
    , stateWorld :: !World
    , stateData :: a
    } deriving (Eq, Ord, Show)

data MoveResult a =
  MoveResult {
      moveResultMove :: !Move
    , moveResultState :: !(State a)
    } deriving (Eq, Ord, Show)

data SetupResult a =
  SetupResult {
      setupResultPunter :: !PunterId
    , setupResultState :: !(State a)
    } deriving (Eq, Ord, Show)

newtype Hostname =
  Hostname {
      getHostname :: Text
    } deriving (Eq, Show)

newtype Port =
  Port {
      getPort :: Int
    } deriving (Eq, Show)

data RobotMove a =
    RobotClaim !a !River
  | RobotPass !a
    deriving (Eq, Ord, Show)

data Robot a =
  Robot {
      robotInit :: Setup -> IO a
    , robotMove :: Gameplay -> State a -> IO (RobotMove a)
    , robotEncode :: a -> Value
    , robotDecode :: Value -> Parser a
    }

fromRobotMove :: State a -> RobotMove a -> MoveResult a
fromRobotMove s0 x =
  case x of
    RobotClaim s r ->
      MoveResult (Claim (statePunter s0) r) (s0 { stateData = s })
    RobotPass s ->
      MoveResult (Pass (statePunter s0)) (s0 { stateData = s })

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
