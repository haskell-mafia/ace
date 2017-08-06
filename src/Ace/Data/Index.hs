{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}

module Ace.Data.Ownership where

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
     indexedSource :: !Index
   , indexedTarget :: !Index
   } deriving (Eq, Ord, Show, Generic)

data Ownership =
    Unclaimed
  | ClaimedBy PunterId
    deriving (Eq, Ord, Show, Generic)

derivingUnbox "Ownership"
  [t| Ownership -> (Bool, PunterId) |]
  [| \case
       ClaimedBy p ->
         (True, p)
       Unclaimed ->
         (False, PunterId 0)
  |]
  [| \(b, p) ->
       if b then
         ClaimedBy p
       else
         Unclaimed
  |]

newtype SiteLedger =
  SiteLedger {
     siteLedger :: Unboxed.Vector SiteId
   } deriving (Eq, Ord, Show, Generic)

asIndexedRiver :: River -> SiteLedger -> Maybe IndexedRiver
asIndexedRiver river sites
  | Just s <- Unboxed.findIndex (== riverSource river) (siteLedger sites)
  , Just t <- Unboxed.findIndex (== riverTarget river) (siteLedger sites)
  = Just $ IndexedRiver (Index s) (Index t)
  | otherwise
  = Nothing

-- | Who owns this river? /O(1)/
--
ownerOf :: IndexedRiver -> Boxed.Vector (Unboxed.Vector Ownership) -> Ownership
ownerOf ix owners =
  let
    s =
      index . indexedSource $ ix
    t =
      index . indexedTarget $ ix
  in
    (owners Boxed.! s) Unboxed.! t

-- | Find river candidates. /O(n^2)/ where /n/ is the number of sites.
--
riverCandidates :: Boxed.Vector (Unboxed.Vector Ownership) -> Unboxed.Vector River
riverCandidates =
  flip Boxed.ifoldl Unboxed.empty $ \acc ix ts ->
    let
      rivers =
        Unboxed.map (makeRiver (SiteId ix) . SiteId). Unboxed.findIndices (== Unclaimed) $ ts
    in
      rivers <> acc
