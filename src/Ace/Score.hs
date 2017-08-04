{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ace.Score (
    calculateScore
  ) where

import           Ace.Data

import qualified Data.Graph.Inductive.Basic as Graph
import qualified Data.Graph.Inductive.Graph as Graph
import           Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Graph.Inductive.Query.SP as Graph
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
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

takeClaim :: Move -> Maybe (PunterId, River)
takeClaim = \case
  Pass _ ->
    Nothing
  Claim pid river ->
    Just (pid, river)

assignRivers :: [Move] -> Gr SiteId River -> Gr SiteId (Maybe PunterId)
assignRivers moves g =
  let
    pids =
      Map.fromList . fmap swap $ mapMaybe takeClaim moves
  in
    Graph.emap (flip Map.lookup pids) g

int :: Int -> Int
int =
  id

calculateScore :: World -> PunterCount -> [Move] -> [PunterScore]
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
              length $ Graph.sp (siteId mid) node g
          in
            Score $ distance * distance
