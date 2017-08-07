{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Ace.Data.Analysis (
    Journey(..)
  , Distance(..)
  , distanceScore

  , routeDistance

  , Bid(..)
  , PunterBid(..)
  , takeClaims
  ) where

import           Ace.Data.Core

import           Data.Binary (Binary)
import           Data.Vector.Binary ()
import qualified Data.Vector.Unboxed as Unboxed
import           Data.Vector.Unboxed.Deriving (derivingUnbox)

import           GHC.Generics (Generic)

import           P

import           X.Text.Show (gshowsPrec)


-- | A journey represents any route between two sites.
--
data Journey =
  Journey {
      journeySource :: !MineId
    , journeyTarget :: !SiteId
    } deriving (Eq, Ord, Generic)

instance Binary Journey

instance Show Journey where
  showsPrec =
    gshowsPrec

derivingUnbox "Journey"
  [t| Journey -> (MineId, SiteId) |]
  [| \(Journey x y) -> (x, y) |]
  [| \(x, y) -> Journey x y |]

data Bid =
    BidClaim
  | BidOption
  | BidSplurge
    deriving (Eq, Ord, Show, Generic)

instance Binary Bid

data PunterBid =
    PunterBid Bid !PunterId !River
    deriving (Eq, Ord, Show, Generic)

instance Binary PunterBid

newtype Distance =
  Distance {
      getDistance :: Int
    } deriving (Eq, Ord, Generic, Num)

instance Binary Distance

instance Show Distance where
  showsPrec =
    gshowsPrec

takeClaims :: PunterMove -> [PunterBid]
takeClaims x =
  case x of
    PunterMove _ Pass ->
      []
    PunterMove pid (Claim river) ->
      [PunterBid BidClaim pid river]
    PunterMove pid (Splurge route) ->
      fmap (PunterBid BidSplurge pid) . Unboxed.toList $ routeRivers route
    PunterMove pid (Option river) ->
      [PunterBid BidOption pid river]

distanceScore :: Distance -> Score
distanceScore x =
  Score (getDistance x * getDistance x)

routeDistance :: Route -> Distance
routeDistance =
  Distance . Unboxed.length . getRoute
