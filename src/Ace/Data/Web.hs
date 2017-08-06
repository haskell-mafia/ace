{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Ace.Data.Web (
    OnlineState(..)
  ) where

import           Ace.Data.Core

import           P

import           GHC.Generics (Generic)

import           X.Text.Show (gshowsPrec)

data OnlineState =
  OnlineState {
      onlineWorld :: !World
    , onlinePunter :: !PunterId
    } deriving (Eq, Generic)

instance Show OnlineState where
  showsPrec =
    gshowsPrec
