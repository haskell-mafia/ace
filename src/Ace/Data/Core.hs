{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
module Ace.Data.Core (
    SiteId(..)
  , MineId(..)

  , Position(..)

  , River
  , riverSource
  , riverTarget
  , makeRiver

  , Score(..)

  , World(..)

  , PunterId(..)
  , PunterCount(..)
  , punters

  , Move(..)
  , PunterMove(..)
  , moveRivers
  , isPass
  , isClaim
  , isOption
  , isSplurge

  , Route(..)
  , makeRoute
  , routeRivers

  , RiverId (..)
  , asRiverId

  , SiteIndex (..)
  , asSiteIndices

  , RiverIndex (..)
  , asRiverIndices

  , IndexedWorld (..)
  , asIndexedWorld
  ) where

import           Data.Binary (Binary)
import           Data.Vector.Binary ()
import qualified Data.Vector.Unboxed as Unboxed
import           Data.Vector.Unboxed.Deriving (derivingUnbox)

import           GHC.Generics (Generic)

import           P

import           X.Text.Show (gshowsPrec)


newtype SiteId =
  SiteId {
      getSiteId :: Int
    } deriving (Eq, Ord, Generic)

instance Binary SiteId

instance Show SiteId where
  showsPrec =
    gshowsPrec

derivingUnbox "SiteId"
  [t| SiteId -> Int |]
  [| getSiteId |]
  [| SiteId |]

newtype MineId =
  MineId {
      getMineId :: SiteId
    } deriving (Eq, Ord, Generic)

instance Binary MineId

instance Show MineId where
  showsPrec =
    gshowsPrec

derivingUnbox "MineId"
  [t| MineId -> SiteId |]
  [| getMineId |]
  [| MineId |]

data Position =
  Position {
      positionX :: !Double
    , positionY :: !Double
    } deriving (Eq, Ord, Generic)

instance Binary Position

instance Show Position where
  showsPrec =
    gshowsPrec

derivingUnbox "Position"
  [t| Position -> (Double, Double) |]
  [| \(Position x y) -> (x, y) |]
  [| \(x, y) -> Position x y |]

data River =
  River {
      riverSource :: !SiteId
    , riverTarget :: !SiteId
    } deriving (Eq, Ord, Generic)

instance Binary River

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
    , worldPositions :: !(Maybe (Unboxed.Vector Position))
    , worldMines :: !(Unboxed.Vector MineId)
    , worldRivers :: !(Unboxed.Vector River)
    } deriving (Eq, Ord, Show, Generic)

instance Binary World

newtype PunterId =
  PunterId {
      punterId :: Int
    } deriving (Eq, Ord, Generic)

derivingUnbox "PunterId"
  [t| PunterId -> Int |]
  [| punterId |]
  [| PunterId |]

instance Binary PunterId where

instance Show PunterId where
  showsPrec =
    gshowsPrec

newtype PunterCount =
  PunterCount {
      punterCount :: Int
    } deriving (Eq, Ord, Generic)

instance Binary PunterCount

instance Show PunterCount where
  showsPrec =
    gshowsPrec

data PunterMove =
  PunterMove {
      punterMoveId :: !PunterId
    , punterMoveValue :: !Move
    } deriving (Eq, Ord, Show, Generic)

instance Binary PunterMove

data Move =
    Claim !River
  | Pass
  | Splurge !Route
  | Option !River
    deriving (Eq, Ord, Show, Generic)

moveRivers :: Move -> [River]
moveRivers m =
  case m of
    Pass ->
      []
    Claim r ->
      [r]
    Splurge r ->
      Unboxed.toList $ routeRivers r
    Option r ->
      [r]

isPass :: Move -> Bool
isPass m =
  case m of
    Pass ->
      True
    Claim _ ->
      False
    Splurge _ ->
      False
    Option _ ->
      False

isClaim :: Move -> Bool
isClaim m =
  case m of
    Pass ->
      False
    Claim _ ->
      True
    Splurge _ ->
      False
    Option _ ->
      False

isSplurge :: Move -> Bool
isSplurge m =
  case m of
    Pass ->
      False
    Claim _ ->
      False
    Splurge _ ->
      True
    Option _ ->
      False

isOption :: Move -> Bool
isOption m =
  case m of
    Pass ->
      False
    Claim _ ->
      False
    Splurge _ ->
      False
    Option _ ->
      True

instance Binary Move where

newtype Score =
  Score {
      getScore :: Int
    } deriving (Eq, Ord, Generic, Num)

instance Show Score where
  showsPrec =
    gshowsPrec

punters :: PunterCount -> [PunterId]
punters n =
  fmap PunterId [0..punterCount n - 1]

newtype Route =
  Route {
      getRoute :: Unboxed.Vector SiteId
    } deriving (Eq, Ord, Show, Generic)

instance Binary Route

makeRoute :: Unboxed.Vector SiteId -> Maybe Route
makeRoute xs =
  if Unboxed.length xs > 1 then
    Just $ Route xs
  else
    Nothing

routeRivers :: Route -> Unboxed.Vector River
routeRivers r =
  let
    sites = getRoute r
    targets = Unboxed.drop 1 sites
    rivers = Unboxed.zip sites targets
  in
    flip Unboxed.map rivers $ \(source, target) ->
      River source target

--------------------------------------------------------------------------------

newtype SiteIndex =
  SiteIndex {
      siteIndex :: Int
    } deriving (Eq, Ord, Generic)

instance Show SiteIndex where
  showsPrec =
    gshowsPrec

derivingUnbox "SiteIndex"
  [t| SiteIndex -> Int |]
  [| siteIndex |]
  [| SiteIndex |]

asSiteIndices :: Unboxed.Vector SiteId -> Unboxed.Vector SiteIndex
asSiteIndices identifiers =
  Unboxed.generate (Unboxed.length identifiers) SiteIndex

newtype RiverId =
  RiverId {
      riverId :: Int
    } deriving (Eq, Ord, Generic)

instance Show RiverId where
  showsPrec =
    gshowsPrec

derivingUnbox "RiverId"
  [t| RiverId -> Int |]
  [| riverId |]
  [| RiverId |]

newtype RiverIndex =
  RiverIndex {
      riverIndex :: Int
    } deriving (Eq, Ord, Generic)

instance Show RiverIndex where
  showsPrec =
    gshowsPrec

derivingUnbox "RiverIndex"
  [t| RiverIndex -> Int |]
  [| riverIndex |]
  [| RiverIndex |]

asRiverId :: River -> RiverId
asRiverId river =
  let
    source =
      getSiteId . riverSource $ river
    target =
      getSiteId . riverTarget $ river
    paired =
      ((source + target) * (source + target + 1) * target) `div` 2
  in
    RiverId paired

asRiverIndices :: Unboxed.Vector RiverId -> Unboxed.Vector RiverIndex
asRiverIndices identifiers =
  Unboxed.generate (Unboxed.length identifiers) RiverIndex

newtype RiverOwner =
  RiverOwner {
      riverOwner :: PunterId
    } deriving (Eq, Ord, Generic)

instance Show RiverOwner where
  showsPrec =
    gshowsPrec

derivingUnbox "RiverOwner"
  [t| RiverOwner -> PunterId |]
  [| riverOwner |]
  [| RiverOwner |]

data IndexedWorld =
  IndexedWorld {
      indexedSites :: !(Unboxed.Vector SiteIndex)
    , indexedMines :: !(Unboxed.Vector SiteIndex)
    , indexedRivers :: !(Unboxed.Vector RiverIndex)
    } deriving (Eq, Ord, Show, Generic)

asIndexedWorld :: World -> IndexedWorld
asIndexedWorld w =
  IndexedWorld
    (asSiteIndices . worldSites $ w)
    (asSiteIndices . Unboxed.map getMineId . worldMines $ w)
    (asRiverIndices . Unboxed.map asRiverId . worldRivers $ w)
