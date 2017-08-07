{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

-- * An alternative to the River API.
--
module Ace.Analysis.Ledger (
    State
  , init
  , claim
  , unclaimed
  , routes
  , update
  , filterPunter
  , reachable
  , filterPunterOrUnclaimed
  ) where

import           Ace.Analysis.Ledger.Index (SiteLedger(..), RiverLedger(..), IndexedRiver(..), Ownership(..))
import qualified Ace.Analysis.Ledger.Index as Ledger
import           Ace.Data.Analysis
import           Ace.Data.Core

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Binary (Binary)
import qualified Data.Vector as Boxed
import qualified Data.Vector.Unboxed as Unboxed

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
{-# INLINE claim #-}

toIndexedRiver :: River -> SiteLedger -> IndexedRiver
toIndexedRiver river sites =
  -- this will only be Nothing if the World is bollocks.
  case Ledger.toIndexedRiver river sites of
    Nothing ->
      Savage.error
        "impossible! river is invalid"
    Just reever ->
      reever

unclaimed :: State -> Set River
unclaimed (State _ rivers) =
  Set.fromList . Unboxed.toList . Ledger.riverCandidates $ rivers

routes :: Set MineId -> State -> Map Journey Route
routes mines (State _ rivers )=
  let
    vec0 =
      Ledger.riverCandidates rivers

    journeys =
      [ Journey m0 (getMineId m1)
        | m0 <- Set.toList mines, m1 <- Set.toList mines, m0 /= m1 ]

    track acc j@(Journey (MineId s0) s1) =
      case Unboxed.find ((== s0) . riverSource) vec0 of
        Nothing ->
          acc
        Just start ->
          case Unboxed.find ((== s1) . riverTarget) vec0 of
            Nothing ->
              acc
            Just end ->
              case Map.lookup j acc of
                Just _ ->
                  acc
                Nothing ->
                  let
                    vec1 =
                      go start end
                    vec2 =
                      Route . Unboxed.cons (riverSource start) . Unboxed.map riverTarget $ vec1
                  in
                    Map.insert j vec2 acc

    go s e =
      if riverTarget s == riverSource e then
        Unboxed.fromList [s, e]
      else
        case Unboxed.find ((== riverTarget s) . riverSource) vec0 of
          Nothing ->
            Unboxed.empty
          Just s' ->
            case Unboxed.find ((== riverSource e) . riverTarget) vec0 of
              Nothing ->
                Unboxed.empty
              Just e' ->
                go s' e'
  in
    foldl' track Map.empty journeys

filterPunterOrUnclaimed :: PunterId -> State -> State
filterPunterOrUnclaimed punter (State sites rivers) =
  let
    f = \case
      Unclaimed ->
        True
      ClaimedBy p ->
        p == punter
      SharedBy p1 p2 ->
        p1 == punter || p2 == punter
      _ ->
        False
    go row =
      row { Ledger.rowLords = Unboxed.filter f . Ledger.rowLords $ row }

  in
    State sites . RiverLedger . Boxed.map go . byLow $ rivers

filterPunter :: PunterId -> State -> State
filterPunter punter (State sites rivers) =
  let
    ownedOrShared = \case
      ClaimedBy p ->
        p == punter
      SharedBy p1 p2 ->
        p1 == punter || p2 == punter
      _ ->
        False
    go row =
      row { Ledger.rowLords = Unboxed.filter ownedOrShared . Ledger.rowLords $ row }

  in
    State sites . RiverLedger . Boxed.map go . byLow $ rivers

reachable :: SiteId -> State -> [SiteId]
reachable s (State sites rivers) =
  Unboxed.toList . Unboxed.map riverTarget . Ledger.riverCandidatesFor s sites $ rivers

--------------------------------------------------------------------------------

update :: [PunterMove] -> State -> State
update moves state0 =
  foldr claim state0 (concatMap takeClaims moves)
