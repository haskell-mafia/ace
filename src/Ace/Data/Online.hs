{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Ace.Data.Online (
    Hostname(..)
  , Port(..)
  ) where

import           P

import           GHC.Generics (Generic)

import           X.Text.Show (gshowsPrec)

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
