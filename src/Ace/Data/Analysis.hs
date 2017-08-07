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

  , PunterClaim(..)
  , takeClaims
  ) where

import           Ace.Data.Core

import           Data.Binary (Binary)
import qualified Data.List as List
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

data PunterClaim =
  PunterClaim !PunterId !River
  deriving (Eq, Ord, Show, Generic)

instance Binary PunterClaim

newtype Distance =
  Distance {
      getDistance :: Int
    } deriving (Eq, Ord, Generic, Num)

instance Binary Distance

instance Show Distance where
  showsPrec =
    gshowsPrec

routeRivers :: Route -> [River]
routeRivers x =
  case Unboxed.toList $ getRoute x of
    [] ->
      []
    xs@(_ : ys) ->
      List.zipWith makeRiver xs ys

takeClaims :: PunterMove -> [PunterClaim]
takeClaims x =
  case x of
    PunterMove _ Pass ->
      []
    PunterMove pid (Claim river) ->
      [PunterClaim pid river]
    PunterMove pid (Splurge route) ->
      fmap (PunterClaim pid) $ routeRivers route
    PunterMove pid (Option river) ->
      [PunterClaim pid river] -- FIX Ace.Analysis.River won't handle this properly

distanceScore :: Distance -> Score
distanceScore x =
  Score (getDistance x * getDistance x)

routeDistance :: Route -> Distance
routeDistance =
  Distance . Unboxed.length . getRoute
