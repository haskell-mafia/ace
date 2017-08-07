{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Ace.Data.Config (
    FutureFlag(..)
  , SplurgeFlag(..)
  , OptionFlag(..)

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

data OptionFlag =
    OptionEnabled
  | OptionDisabled
    deriving (Eq, Show, Ord, Bounded, Enum, Generic)

instance Binary OptionFlag where

data Config =
  Config {
      futureConfig :: FutureFlag
    , splurgeConfig :: SplurgeFlag
    , optionConfig :: OptionFlag
    } deriving (Eq, Show, Generic)

defaultConfig :: Config
defaultConfig =
  Config
    FutureDisabled
    SplurgeDisabled
    OptionDisabled
