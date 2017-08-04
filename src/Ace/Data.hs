{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

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
  , PunterScore(..)
  , State(..)
  , MoveResult(..)
  , MoveResultServer(..)
  , MoveRequestServer(..)
  , SetupResult(..)
  , SetupResultServer(..)
  , Hostname(..)
  , Port(..)

  , Robot(..)
  , RobotMove(..)
  , fromRobotMove

  , Settings(..)
  , FuturesFlag (..)

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

import           GHC.Generics (Generic)

import           System.IO (IO)

import           X.Text.Show (gshowsPrec)


newtype SiteId =
  SiteId {
      siteId :: Int
    } deriving (Eq, Ord, Generic)

instance Show SiteId where
  showsPrec =
    gshowsPrec

derivingUnbox "SiteId"
  [t| SiteId -> Int |]
  [| siteId |]
  [| SiteId |]

data River =
  River {
      riverSource :: !SiteId
    , riverTarget :: !SiteId
    } deriving (Eq, Ord, Generic)

instance Show River where
  showsPrec =
    gshowsPrec

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
    } deriving (Eq, Ord, Generic)

instance Show Punter where
  showsPrec =
    gshowsPrec

newtype PunterId =
  PunterId {
      punterId :: Int
    } deriving (Eq, Ord, Generic)

instance Show PunterId where
  showsPrec =
    gshowsPrec

newtype PunterCount =
  PunterCount {
      punterCount :: Int
    } deriving (Eq, Ord, Generic)

instance Show PunterCount where
  showsPrec =
    gshowsPrec

data Move =
    Claim !PunterId !River
  | Pass !PunterId
    deriving (Eq, Ord, Show)

data OfflineRequest a =
    OfflineSetup !Setup
  | OfflineGameplay !Gameplay !(State a)
  | OfflineScoring !(Stop (State a)) !(State a)
    deriving (Eq, Ord, Show)

data Setup =
  Setup {
      setupPunter :: !PunterId
    , setupPunterCount :: !PunterCount
    , setupWorld :: !World
    } deriving (Eq, Ord, Show)

data Stop a =
  Stop {
      stopMoves :: ![Move]
    , stopScores :: ![PunterScore]
    , stopState :: !(Maybe a)
    } deriving (Eq, Ord, Show)

data MovesOrStop a =
    JustMoves ![Move]
  | JustStop !(Stop a)
    deriving (Eq, Show)

newtype Gameplay =
  Gameplay {
      gameplay :: [Move]
    } deriving (Eq, Ord, Show)

newtype Score =
  Score {
      score :: Int
    } deriving (Eq, Ord, Generic, Num)

instance Show Score where
  showsPrec =
    gshowsPrec

data PunterScore =
  PunterScore {
      scorePunter :: !PunterId
    , scoreValue :: !Score
    } deriving (Eq, Ord, Generic)

instance Show PunterScore where
  showsPrec =
    gshowsPrec

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

data MoveResultServer a =
  MoveResultServer {
      moveResultServerMove :: !Move
    , moveResultServerState :: !a
    } deriving (Eq, Ord, Show)

data MoveRequestServer a =
  MoveRequestServer {
      moveRequestServerMoves :: ![Move]
    , moveRequestServerState :: !a
    } deriving (Eq, Ord, Show)

data SetupResult a =
  SetupResult {
      setupResultPunter :: !PunterId
    , setupResultState :: !(State a)
    } deriving (Eq, Ord, Show)

data SetupResultServer a =
  SetupResultServer {
      setupResultServerPunter :: !PunterId
    , setupResultServerState :: !a
    } deriving (Eq, Ord, Show)

newtype Hostname =
  Hostname {
      getHostname :: Text
    } deriving (Eq, Generic)

instance Show Hostname where
  showsPrec =
    gshowsPrec

newtype Port =
  Port {
      getPort :: Int
    } deriving (Eq, Generic)

instance Show Port where
  showsPrec =
    gshowsPrec

data RobotMove a =
    RobotClaim !a !River
  | RobotPass !a
    deriving (Eq, Ord, Show)

data Robot a =
  Robot {
      robotLabel :: Text
    , robotInit :: Setup -> IO a
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

data Settings =
  Settings {
      futuresSettings :: FuturesFlag
    } deriving (Eq, Show)

data FuturesFlag =
    FuturesEnabled
  | FuturedDisabled
    deriving (Eq, Show)

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

derivingUnbox "Score"
  [t| Score -> Int |]
  [| score |]
  [| Score |]
