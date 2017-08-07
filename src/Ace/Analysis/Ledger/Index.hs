{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}

module Ace.Analysis.Ledger.Index (
    Index (..)
  , IndexedRiver (..)
  , toIndexedRiver

  , Ownership (..)
  , SiteLedger (..)
  , RiverLedger (..)
  , emptyRiverLedger
  , ownerOfIndexed
  , mark
  , markClaimedOnly
  , markUnclaimedOnly
  , riverCandidates
  , riverCandidatesFor
  ) where

import           Ace.Data.Core

import           GHC.Generics (Generic)

import           P

import           Data.Binary (Binary)
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
  | SharedBy PunterId PunterId
    deriving (Eq, Ord, Show, Generic)

instance Binary Ownership

derivingUnbox "Ownership"
  [t| Ownership -> (Int, PunterId, PunterId) |]
  [| \case
       Bogus ->
         (0, PunterId 0, PunterId 0)
       Unclaimed ->
         (1, PunterId 0, PunterId 0)
       ClaimedBy p ->
         (2, p, PunterId 0)
       SharedBy p1 p2 ->
         (3, p1, p2)
  |]
  [| \(n, p1, p2) ->
       case n of
         0 ->
           Bogus
         1 ->
           Unclaimed
         2 ->
           ClaimedBy p1
         _ ->
           SharedBy p1 p2
  |]

newtype SiteLedger =
  SiteLedger {
     siteLedger :: Unboxed.Vector SiteId
   } deriving (Eq, Ord, Show, Generic)


instance Binary SiteLedger

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
--   [ 0 ~ 1 claimed!
--   , 0 ~ 2
--   , 1 ~ 2 claimed!
--   , 1 ~ 3
--   , 2 ~ 4
--   ]
--
-- Matrix:
--
-- [ 1 [ _ C U _ _ ]
-- , 1 [ C _ C U _ ]
-- , 2 [ U C _ _ U ]
-- , 1 [ _ U _ _ _ ]
-- , 1 [ _ _ U _ _ ]
-- ]
--
-- Q: what are the candidates for node 2?
-- A: matrix[2] has unclaimed count = 0 so none
--
-- Q: what are the candidates for node 1?
-- A: matrix[1] has count = 1 so scan for all the unclaimed.
--    because it's a triangular matrix, we scan from 1 onwards.
--    we see: C U, so 1 ~ 3 is available.
--
-- Q: claim 1 ~ 3
-- A: matrix[1] count = 0, matrix[1][3] = C
--    matrix[3] count = 0, matrix[3][1] = C
--
-- [ 1 [ _ C U _ _ ]
-- , 0 [ C _ C C _ ]
-- , 2 [ U C _ _ U ]
-- , 0 [ _ C _ _ _ ]
-- , 1 [ _ _ U _ _ ]
-- ]
--
-- Q: whare the candidates now?
-- A: matrix[0] scan i > 0 => [ 0 ~ 2 ]
--    matrix[1] scan i > 1 => [ ]
--    matrix[2] scan i > 2 => [ 2 ~ 4 ]
--    matrix[3] scan i > 3 => [ ]
--    matrix[4] scan i > 3 => [ ]
--    ==> [ 0 ~ 2, 2 ~ 4 ]
--

data RiverLedger =
  RiverLedger {
    byLow :: !(Boxed.Vector Row)
  } deriving (Eq, Ord, Show, Generic)

instance Binary RiverLedger

data Row =
  Row {
    rowUnclaimed :: !Int
    -- ^ How many rivers connecting to this index are unclaimed?
  , rowShareable :: !Int
    -- ^ How many are claimed by can be shared?
  , rowLords :: !(Unboxed.Vector Ownership)
    -- ^ Who owns rivers connecting to this index?
  } deriving (Eq, Ord, Show, Generic)

instance Binary Row

-- | (x ~ y) is a river in the world ==>
--     ledger[x][y] == Unclaimed && ledger[y][x] == Unclaimed
--
--   (x ~ y) is not a river ==>
--     ledger[x][y] == Bogus && ledger[y][x] = Bogus
--
emptyRiverLedger :: World -> Maybe RiverLedger
emptyRiverLedger w = do
  let
    bound =
      Unboxed.length (worldSites w)

    nans =
      Boxed.replicate bound $ Row 0 0 . Unboxed.replicate bound $ Bogus

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
              rowUnclaimed = rowUnclaimed ind + 1
            , rowLords = Unboxed.unsafeUpd (rowLords ind) [(ix1, Unclaimed)]
          })]

    sites =
      SiteLedger . worldSites $ w

  indexed <- Unboxed.mapM (flip toIndexedRiver sites) . worldRivers $ w

  Just . RiverLedger $
    Unboxed.foldl (sleepy indexedLower indexedHigher) nans $ indexed
{-# INLINE emptyRiverLedger #-}

-- | ledger[x][y] == ledger[y][x]
--
ownerOfIndexed :: IndexedRiver -> RiverLedger -> Ownership
ownerOfIndexed (IndexedRiver (Index lower) (Index higher)) (RiverLedger ls) =
  Unboxed.unsafeIndex (rowLords (Boxed.unsafeIndex ls lower)) higher
{-# INLINE ownerOfIndexed #-}

-- | mark (x ~ y) when
--
--   ledger[x][y] == Bogus ==>
--     nothing happens
--
--   ledger[x][y] == Unclaimed ==>
--     ledger[x][y] == ledger[y][x] == ClaimedBy p
--
--   ledger[x][y] == ClaimedBy p1 ==>
--     ledger[x][y] == ledger[y][x] == SharedBy p1 p
--
--   ledger[x][y] == SharedBy p1 p2 ==>
--     nothing happens
--
mark :: PunterId -> IndexedRiver -> RiverLedger -> RiverLedger
mark p (IndexedRiver (Index lower) (Index higher)) (RiverLedger rivers) =
  let
    go ix xs =
      let
        upd0 =
          xs { rowUnclaimed = rowUnclaimed xs - 1
             , rowShareable = rowShareable xs + 1
             , rowLords = Unboxed.unsafeUpd (rowLords xs) [(ix, ClaimedBy p)] }

        upd1 p1 =
          xs { rowShareable = rowShareable xs - 1
             , rowLords = Unboxed.unsafeUpd (rowLords xs) [(ix, SharedBy p1 p)] }

      in
        case Unboxed.unsafeIndex (rowLords xs) ix of
          Bogus ->
            xs
          Unclaimed ->
            upd0
          ClaimedBy p1 ->
            upd1 p1
          SharedBy _ _ ->
            xs

    updUpper =
      [ (higher, go lower (Boxed.unsafeIndex rivers higher)) ]

    updLower =
      [ (lower, go higher (Boxed.unsafeIndex rivers lower)) ]

  in
    RiverLedger . flip Boxed.unsafeUpd updLower . flip Boxed.unsafeUpd updUpper $ rivers
{-# INLINE mark #-}


-- | claim (x ~ y) when
--
--   ledger[x][y] == Bogus ==>
--     nothing happens
--
--   ledger[x][y] == Unclaimed ==>
--     ledger[x][y] == ledger[y][x] == ClaimedBy p
--
--   ledger[x][y] == ClaimedBy p1 ==>
--     nothing happens
--
--   ledger[x][y] == SharedBy p1 p2 ==>
--     nothing happens
--
markUnclaimedOnly :: PunterId -> IndexedRiver -> RiverLedger -> RiverLedger
markUnclaimedOnly p (IndexedRiver (Index lower) (Index higher)) (RiverLedger rivers) =
  let
    go ix xs =
      let
        upd =
          xs { rowUnclaimed = rowUnclaimed xs - 1
             , rowShareable = rowShareable xs + 1
             , rowLords = Unboxed.unsafeUpd (rowLords xs) [(ix, ClaimedBy p)] }
      in
        case Unboxed.unsafeIndex (rowLords xs) ix of
          Bogus ->
            xs
          Unclaimed ->
            upd
          ClaimedBy _ ->
            xs
          SharedBy _ _ ->
            xs

    updUpper =
      [ (higher, go lower (Boxed.unsafeIndex rivers higher)) ]

    updLower =
      [ (lower, go higher (Boxed.unsafeIndex rivers lower)) ]

  in
    RiverLedger . flip Boxed.unsafeUpd updLower . flip Boxed.unsafeUpd updUpper $ rivers

-- | share (x ~ y) when
--
--   ledger[x][y] == Bogus ==>
--     nothing happens
--
--   ledger[x][y] == Unclaimed ==>
--     nothing happens
--
--   ledger[x][y] == ClaimedBy p1 ==>
--     ledger[x][y] == ledger[y][x] == SharedBy p1 p
--
--   ledger[x][y] == SharedBy p1 p2 ==>
--     nothing happens
--
markClaimedOnly :: PunterId -> IndexedRiver -> RiverLedger -> RiverLedger
markClaimedOnly p (IndexedRiver (Index lower) (Index higher)) (RiverLedger rivers) =
  let
    go ix xs =
      let
        upd p1 =
          xs { rowShareable = rowShareable xs - 1
             , rowLords = Unboxed.unsafeUpd (rowLords xs) [(ix, SharedBy p1 p)] }
      in
        case Unboxed.unsafeIndex (rowLords xs) ix of
          Bogus ->
            xs
          Unclaimed ->
            xs
          ClaimedBy p1 ->
            upd p1
          SharedBy _ _ ->
            xs
    updUpper =
      [ (higher, go lower (Boxed.unsafeIndex rivers higher)) ]

    updLower =
      [ (lower, go higher (Boxed.unsafeIndex rivers lower)) ]

  in
    RiverLedger . flip Boxed.unsafeUpd updLower . flip Boxed.unsafeUpd updUpper $ rivers

-- | Find all river candidates. /O(n^2)/ where /n/ is the number of sites.
--
riverCandidates :: RiverLedger -> Unboxed.Vector River
riverCandidates (RiverLedger as) =
  let
    makeRivers ix row f =
      Unboxed.map (makeRiver_ ix) .
      findIndicesFrom (ix + 1) f .
      rowLords $
        row

    claimableIn ix row =
      if rowUnclaimed row == 0 then
        Unboxed.empty
      else
        makeRivers ix row (== Unclaimed)

    shareableIn ix row =
      if rowShareable row == 0 then
        Unboxed.empty
      else
        makeRivers ix row $ \case
          ClaimedBy _ ->
            True
          _ ->
            False

    rivers =
      flip Boxed.ifoldl Unboxed.empty $ \acc ix row ->
        acc Unboxed.++ claimableIn ix row Unboxed.++ shareableIn ix row
  in
    rivers as
{-# INLINE riverCandidates #-}

-- | Find candidates for a single vertex. Worst case /O(n)/.
--
riverCandidatesFor :: SiteId -> SiteLedger -> RiverLedger -> Unboxed.Vector River
riverCandidatesFor site sites (RiverLedger as) =
  case siteIndex site sites of
    Nothing ->
      Unboxed.empty

    Just (Index ix) ->
      let
        row =
          Boxed.unsafeIndex as ix

        makeRivers f =
          Unboxed.map (makeRiver_ ix) .
          findIndicesFrom (ix + 1) f .
          rowLords $
            row

        claimable =
          if rowUnclaimed row == 0 then
            Unboxed.empty
          else
            makeRivers (== Unclaimed)

        shareable =
          if rowShareable row == 0 then
            Unboxed.empty
          else
            makeRivers $ \case
              ClaimedBy _ ->
                True
              _ ->
                False
      in
        claimable Unboxed.++ shareable

findIndicesFrom :: Unboxed.Unbox a => Int -> (a -> Bool) -> Unboxed.Vector a -> Unboxed.Vector Int
findIndicesFrom x f =
  Unboxed.findIndices f . Unboxed.unsafeDrop x

makeRiver_ :: Int -> Int -> River
makeRiver_ x =
  makeRiver (SiteId x) . SiteId
