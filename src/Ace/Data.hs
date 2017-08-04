{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Ace.Data (
    SiteId(..)
  , River(..)
  , World(..)
  ) where

import           P

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
