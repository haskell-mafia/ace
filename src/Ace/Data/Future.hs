{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Ace.Data.Future (
    FuturesFlag(..)
  , Future(..)
  ) where

import           Ace.Data.Core

import           Data.Binary (Binary)

import           GHC.Generics (Generic)

import           P


data FuturesFlag =
    FuturesEnabled
  | FuturesDisabled
    deriving (Eq, Show, Ord, Bounded, Enum, Generic)

instance Binary FuturesFlag where

data Future =
  Future {
      futureSource :: !SiteId
    , futureTarget :: !SiteId
    } deriving (Eq, Show, Generic)

instance Binary Future where
