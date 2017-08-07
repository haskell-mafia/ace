{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- * An alternative to the River API.
--
module Ace.Analysis.Ledger (
    State
  , init
  , claim
  , lookup
  ) where

import           Ace.Analysis.Ledger.Index (SiteLedger(..), RiverLedger(..), IndexedRiver(..), Ownership(..))
import qualified Ace.Analysis.Ledger.Index as Ledger
import           Ace.Data.Analysis
import           Ace.Data.Core

import           Data.Binary (Binary)

import           GHC.Generics (Generic)

import           P

import qualified Prelude as Savage


data State =
  State !SiteLedger !RiverLedger
    deriving (Eq, Show, Generic)

instance Binary State

init :: World -> State
init w =
   -- this will only be Nothing if the World is bollocks.
  case Ledger.emptyRiverLedger w of
    Nothing ->
      Savage.error
        "impossible! the world is invalid (rivers connecting nonexistent sites)"
    Just ledger ->
      flip State ledger . SiteLedger . worldSites $ w

claim :: PunterBid -> State -> State
claim (PunterBid _ punter river) (State sites rivers) =
  State sites . Ledger.mark punter (toIndexedRiver river sites) $ rivers

lookup :: River -> State -> Ownership
lookup river (State sites rivers )=
  Ledger.ownerOfIndexed (toIndexedRiver river sites) rivers

toIndexedRiver :: River -> SiteLedger -> IndexedRiver
toIndexedRiver river sites =
  -- this will only be Nothing if the World is bollocks.
  case Ledger.toIndexedRiver river sites of
    Nothing ->
      Savage.error
        "impossible! river is invalid"
    Just reever ->
      reever
