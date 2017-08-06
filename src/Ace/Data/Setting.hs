{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Ace.Data.Setting (
    FuturesFlag(..)

  , Settings(..)
  , defaultSettings
  ) where

import           Data.Binary (Binary)

import           GHC.Generics (Generic)

import           P


data FuturesFlag =
    FuturesEnabled
  | FuturesDisabled
    deriving (Eq, Show, Ord, Bounded, Enum, Generic)

instance Binary FuturesFlag where

data Settings =
  Settings {
      futuresSettings :: FuturesFlag
    } deriving (Eq, Show, Generic)

defaultSettings :: Settings
defaultSettings =
  Settings
    FuturesDisabled
