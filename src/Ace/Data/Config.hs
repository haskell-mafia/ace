{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Ace.Data.Config (
    FutureFlag(..)
  , SplurgeFlag(..)

  , Config(..)
  , defaultConfig
  ) where

import           Data.Binary (Binary)

import           GHC.Generics (Generic)

import           P


data FutureFlag =
    FutureEnabled
  | FutureDisabled
    deriving (Eq, Show, Ord, Bounded, Enum, Generic)

instance Binary FutureFlag where

data SplurgeFlag =
    SplurgeEnabled
  | SplurgeDisabled
    deriving (Eq, Show, Ord, Bounded, Enum, Generic)

instance Binary SplurgeFlag where

data Config =
  Config {
      futureConfig :: FutureFlag
    , splurgeConfig :: SplurgeFlag
    } deriving (Eq, Show, Generic)

defaultConfig :: Config
defaultConfig =
  Config
    FutureDisabled
    SplurgeDisabled
