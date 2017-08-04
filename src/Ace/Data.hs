{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Ace.Data (
    SiteId(..)
  , River(..)
  , World(..)
  , Punter(..)
  , PunterId(..)
  , PunterCount(..)
  , renderSite
  , renderRiver
  , renderWorld
  , renderPunterId
  , renderPunterCount
  ) where

import           P

import qualified Data.List as List
import qualified Data.Text as Text

import qualified Data.Vector.Unboxed as Unboxed
import           Data.Vector.Unboxed.Deriving (derivingUnbox)


newtype SiteId =
  SiteId {
      siteId :: Int
    } deriving (Eq, Ord, Show)

derivingUnbox "SiteId"
  [t| SiteId -> Int |]
  [| siteId |]
  [| SiteId |]

data River =
  River {
      riverSource :: !SiteId
    , riverTarget :: !SiteId
    } deriving (Eq, Ord, Show)

derivingUnbox "River"
  [t| River -> (SiteId, SiteId) |]
  [| \(River x y) -> (x, y) |]
  [| \(x, y) -> River x y |]

data World =
  World {
      worldSites :: !(Unboxed.Vector SiteId)
    , worldMines :: !(Unboxed.Vector SiteId)
    , worldRivers :: !(Unboxed.Vector River)
    } deriving (Eq, Ord, Show)

newtype Punter =
  Punter {
      renderPunter :: Text
    } deriving (Eq, Ord, Show)

newtype PunterId =
  PunterId {
      punterId :: Int
    } deriving (Eq, Ord, Show)

newtype PunterCount =
  PunterCount {
      punterCount :: Int
    } deriving (Eq, Ord, Show)

renderSite :: SiteId -> Text
renderSite =
  Text.pack . show . siteId

renderMine :: SiteId -> Text
renderMine x =
  "[" <> renderSite x <> "]"

renderRiver :: [SiteId] -> River -> Text
renderRiver mines (River x y) =
  let
    render a
      | List.elem a mines =
          renderMine a
      | otherwise =
          renderSite a

  in
    render x <> " ~ " <> render y

renderWorld :: World -> Text
renderWorld world =
   Text.intercalate "\n" .
   fmap (renderRiver . Unboxed.toList . worldMines $ world) .
   Unboxed.toList .
   worldRivers $ world

renderPunterId :: PunterId -> Text
renderPunterId =
  Text.pack . show . punterId

renderPunterCount :: PunterCount -> Text
renderPunterCount =
  Text.pack . show . punterCount
