{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ace.Data.Index (
    Index (..)
  , IndexedRiver (..)
  , SiteLedger (..)
  , RiverLedger (..)
  , emptyRiverLedger
  , riverCandidates
  , riverCandidatesFor
  , markAsClaimed
  , ownerOfIndexed
  ) where

import           Ace.Data.Core

import           GHC.Generics (Generic)

import           P

import qualified Data.Vector as Boxed
import qualified Data.Vector.Unboxed as Unboxed
import           Data.Vector.Unboxed.Deriving (derivingUnbox)


newtype Index =
  Index {
     index :: Int
   } deriving (Eq, Ord, Show, Generic)

derivingUnbox "Index"
  [t| Index -> Int |]
  [| index |]
  [| Index |]

data IndexedRiver =
  IndexedRiver {
     indexedLower :: !Index
   , indexedHigher :: !Index
   } deriving (Eq, Ord, Show, Generic)

derivingUnbox "IndexedRiver"
  [t| IndexedRiver -> (Index, Index) |]
  [| \(IndexedRiver x y) -> (x, y) |]
  [| \(x, y) -> IndexedRiver x y |]

data Ownership =
    Bogus
  | Unclaimed
  | ClaimedBy PunterId
    deriving (Eq, Ord, Show, Generic)

derivingUnbox "Ownership"
  [t| Ownership -> (Int, PunterId) |]
  [| \case
       Bogus ->
         (0, PunterId 0)
       Unclaimed ->
         (1, PunterId 0)
       ClaimedBy p ->
         (2, p)
  |]
  [| \(n, p) ->
       case n of
         0 ->
           Bogus
         1 ->
           Unclaimed
         _ ->
           ClaimedBy p
  |]

newtype SiteLedger =
  SiteLedger {
     siteLedger :: Unboxed.Vector SiteId
   } deriving (Eq, Ord, Show, Generic)

siteIndex :: SiteId -> SiteLedger -> Maybe Index
siteIndex site =
  fmap Index . Unboxed.findIndex (== site) . siteLedger

toIndexedRiver :: River -> SiteLedger -> Maybe IndexedRiver
toIndexedRiver x ss =
  let
    source =
      riverSource x

    target =
      riverTarget x

    (lower, higher) =
      if source < target then
        (source, target)
      else
        (target, source)

    sites =
      siteLedger ss

    river
      | Just a <- Unboxed.findIndex (== lower) sites
      , Just b <- Unboxed.findIndex (== higher) sites
      = Just $ IndexedRiver (Index a) (Index b)
      | otherwise
      = Nothing
  in
    river
{-# INLINE toIndexedRiver #-}

--------------------------------------------------------------------------------

--
-- Rivers (low ~ high)
--   [ 0 ~ 0
--   , 0 ~ 1 claimed!
--   , 0 ~ 2
--   , 1 ~ 2 claimed!
--   , 1 ~ 3
--   ]
--
-- Matrix:
--
-- [ 2 [ U C U _ ]
-- , 1 [ _ _ C U ]
-- , 0 [ _ _ _ _ ]
-- , 0 [ _ _ _ _ ]
-- ]
--
-- Q: what are the candidates for node 2?
-- A: matrix[2] has unclaimed count = 0 so none
--
-- Q: what are the candidates for node 1?
-- A: matrix[1] has count = 1 so scan for all the unclaimed.
--    because it's a upper triangular matrix, only need to scan from 1 onwards.
--    we see: C U, so 1 ~ 3 is available.
--

data RiverLedger =
  RiverLedger {
    byLow :: !(Boxed.Vector Indices)
  } deriving (Eq, Ord, Show, Generic)

data Indices =
  Indices {
    indexUnclaimed :: !Int
    -- ^ How many rivers connecting to this index are unclaimed?
  , indexOwns :: !(Unboxed.Vector Ownership)
    -- ^ Who owns rivers connecting to this index?
  } deriving (Eq, Ord, Show, Generic)

-- This will only be Nothing if the World is bollocks.
emptyRiverLedger :: World -> Maybe RiverLedger
emptyRiverLedger w = do
  let
    bound =
      Unboxed.length (worldSites w)

    nans =
      Boxed.replicate bound $
        Indices 0 . Unboxed.replicate bound $
          Bogus

    sleepy f g vec0 river =
      let
        ix0 =
          index . f $ river
        ix1 =
          index . g $ river
        ind =
          Boxed.unsafeIndex vec0 ix0
      in
        Boxed.unsafeUpd vec0 $
          [(ix0, ind {
              indexUnclaimed = indexUnclaimed ind + 1
            , indexOwns = Unboxed.unsafeUpd (indexOwns ind) [(ix1, Unclaimed)]
          })]

    sites =
      SiteLedger . worldSites $ w

  indexed <- Unboxed.mapM (flip toIndexedRiver sites) . worldRivers $ w

  Just . RiverLedger $
    Unboxed.foldl (sleepy indexedLower indexedHigher) nans $ indexed

ownerOfIndexed :: IndexedRiver -> RiverLedger -> Ownership
ownerOfIndexed (IndexedRiver (Index lower) (Index higher)) (RiverLedger ls) =
  Unboxed.unsafeIndex (indexOwns (Boxed.unsafeIndex ls lower)) higher
{-# INLINE ownerOfIndexed #-}

markAsClaimed :: PunterId -> IndexedRiver -> RiverLedger -> RiverLedger
markAsClaimed p (IndexedRiver (Index lower) (Index higher)) (RiverLedger rivers) =
  let
    go ix xs =
      case Unboxed.unsafeIndex (indexOwns xs) ix of
        Bogus ->
          -- TODO should we actually blow up violently?
          xs
        Unclaimed ->
          Indices (indexUnclaimed xs - 1) .
          flip Unboxed.unsafeUpd [(ix, ClaimedBy p)] .
          indexOwns $ xs
        ClaimedBy _ ->
          -- TODO should we actually blow up violently?
          xs
  in
    RiverLedger $
      flip Boxed.unsafeUpd [(higher, go lower . Boxed.unsafeIndex rivers $ higher)] $
      flip Boxed.unsafeUpd [(lower, go higher . Boxed.unsafeIndex rivers $ lower)] $
        rivers
{-# INLINE markAsClaimed #-}

-- | Find river candidates. /O(n^2)/ where /n/ is the number of sites.
--
riverCandidates :: RiverLedger -> Unboxed.Vector River
riverCandidates =
  let
    go =
      flip Boxed.ifoldl Unboxed.empty $ \acc lowIx highIxs ->
        if indexUnclaimed highIxs == 0 then
          acc
        else
          (acc Unboxed.++) .
          Unboxed.map (connect lowIx) .
          Unboxed.findIndices (== Unclaimed) .
          indexOwns $ highIxs
  in
    go . byLow
{-# INLINE riverCandidates #-}

riverCandidatesFor :: SiteId -> SiteLedger -> RiverLedger -> Unboxed.Vector River
riverCandidatesFor site sites (RiverLedger as) =
  case siteIndex site sites of
    Nothing ->
      Unboxed.empty
    Just (Index ix) ->
      if indexUnclaimed (Boxed.unsafeIndex as ix) == 0 then
        Unboxed.empty
      else
        Unboxed.map (connect ix) .
        Unboxed.findIndices (== Unclaimed) . -- TODO only need to scan from ix onwards (see comment above)
        indexOwns . Boxed.unsafeIndex as $ ix

connect :: Int -> Int -> River
connect x =
  makeRiver (SiteId x) . SiteId
