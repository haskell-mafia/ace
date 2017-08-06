{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Ace.Data.Future (
    Future(..)
  ) where

import           Ace.Data.Core

import           Data.Binary (Binary)

import           GHC.Generics (Generic)

import           P


data Future =
  Future {
      futureSource :: !SiteId
    , futureTarget :: !SiteId
    } deriving (Eq, Show, Generic)

instance Binary Future where
