{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
module Ace.Score (
    calculateScore
  , fromWorld
  , assignRivers
  ) where

import           Ace.Data.Core
import           Ace.Data.Protocol

import qualified Data.Graph.Inductive.Basic as Graph
import qualified Data.Graph.Inductive.Graph as Graph
import           Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Graph.Inductive.Query.SP as Graph
import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as Unboxed

import           P


fromWorld :: World -> Gr SiteId River
fromWorld world =
  let
    nodes =
      fmap (\x -> (siteId x, x)) . Unboxed.toList $ worldSites world

    edges =
      fmap (\r -> (siteId (riverSource r), siteId (riverTarget r), r)) . Unboxed.toList $ worldRivers world
  in
    Graph.undir $ Graph.mkGraph nodes edges

takeClaimX :: PunterMove -> Maybe (PunterId, River)
takeClaimX = \case
  PunterMove _ Pass ->
    Nothing
  PunterMove pid (Claim river) ->
    Just (pid, river)

assignRivers :: [PunterMove] -> Gr SiteId River -> Gr SiteId (Maybe PunterId)
assignRivers moves g =
  let
    pids =
      Map.fromList . fmap swap $ mapMaybe takeClaimX moves
  in
    Graph.emap (flip Map.lookup pids) g

int :: Int -> Int
int =
  id

calculateScore :: World -> PunterCount -> [PunterMove] -> [PunterScore]
calculateScore world n moves =
  let
    pids =
      fmap PunterId [0..punterCount n - 1]

    mines =
      Unboxed.toList $ worldMines world

    g0 =
      assignRivers moves (fromWorld world)
  in
    with pids $ \pid ->
      let
        g =
          Graph.emap (const $ int 1) $ Graph.elfilter (== Just pid) g0
      in
        PunterScore pid . sum . fmap sum $
        with mines $ \mid ->
        with (Graph.nodes g) $ \node ->
          let
            distance =
              case Graph.sp (siteId mid) node g of
                [] ->
                  0
                xs ->
                  length xs - 1
          in
            Score $ distance * distance
