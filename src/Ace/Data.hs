{-# LANGUAGE NoImplicitPrelude #-}
module Ace.Data (
    SiteId(..)
  , River(..)
  , World(..)
  ) where

import           P


newtype SiteId =
  SiteId {
      siteId :: Int
    } deriving (Eq, Ord, Show)

data River =
  River {
      riverSource :: SiteId
    , riverTarget :: SiteId
    } deriving (Eq, Ord, Show)

data World =
  World {
      worldSites :: [SiteId]
    , worldMines :: [SiteId]
    , worldRivers :: [River]
    } deriving (Eq, Ord, Show)
