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

  , River
  , riverSource
  , riverTarget
  , makeRiver

  , World(..)

  , PunterId(..)
  , PunterCount(..)

  , Move(..)
  , PunterMove(..)
  ) where

import           P

import           Data.Binary (Binary)
import           Data.Vector.Binary ()
import qualified Data.Vector.Unboxed as Unboxed
import           Data.Vector.Unboxed.Deriving (derivingUnbox)

import           GHC.Generics (Generic)

import           X.Text.Show (gshowsPrec)


newtype SiteId =
  SiteId {
      siteId :: Int
    } deriving (Eq, Ord, Generic)

instance Binary SiteId where

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

instance Binary River where

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
    } deriving (Eq, Ord, Show, Generic)

instance Binary World where

newtype PunterId =
  PunterId {
      punterId :: Int
    } deriving (Eq, Ord, Generic)

instance Binary PunterId where

instance Show PunterId where
  showsPrec =
    gshowsPrec


newtype PunterCount =
  PunterCount {
      punterCount :: Int
    } deriving (Eq, Ord, Generic)

instance Binary PunterCount where

instance Show PunterCount where
  showsPrec =
    gshowsPrec

data PunterMove =
  PunterMove {
      punterMoveId :: PunterId
    , punterMoveValue :: Move
    } deriving (Eq, Ord, Show, Generic)

instance Binary PunterMove where

data Move =
    Claim !River
  | Pass
    deriving (Eq, Ord, Show, Generic)

instance Binary Move where
