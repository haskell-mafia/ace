{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}

module Ace.Score (
    calculateScore
  , fromWorld
  , takeClaim
  , assignRivers
  , Route (..)
  , bestRoute
  ) where

import           Ace.Data.Core
import           Ace.Data.Protocol

import qualified Data.List as List

import qualified Data.Graph.Inductive.Basic as Graph
import qualified Data.Graph.Inductive.Graph as Graph
import           Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Graph.Inductive.Query.SP as Graph
import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as Unboxed

import           GHC.Generics (Generic)

import           P

import           X.Text.Show (gshowsPrec)

fromWorld :: World -> Gr SiteId River
fromWorld world =
  let
    nodes =
      fmap (\x -> (siteId x, x)) . Unboxed.toList $ worldSites world

    edges =
      fmap (\r -> (siteId (riverSource r), siteId (riverTarget r), r)) . Unboxed.toList $ worldRivers world
  in
    Graph.undir $ Graph.mkGraph nodes edges

takeClaim :: PunterMove -> Maybe (PunterId, River)
takeClaim = \case
  PunterMove _ Pass ->
    Nothing
  PunterMove pid (Claim river) ->
    Just (pid, river)

assignRivers :: [PunterMove] -> Gr SiteId River -> Gr SiteId (Maybe PunterId)
assignRivers moves g =
  let
    pids =
      Map.fromList . fmap swap $ mapMaybe takeClaim moves
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

--------------------------------------------------------------------------------

data Route =
  Route {
      routeMine :: !SiteId
    , routeValue :: !Score
    , routePath :: !(Unboxed.Vector SiteId)
    } deriving (Eq, Ord, Generic)

instance Show Route where
  showsPrec =
    gshowsPrec

bestRoute :: [PunterMove] -> Maybe PunterId -> World -> Route
bestRoute moves pid world =
  let
    mines =
      Unboxed.toList $ worldMines world

    g0 =
      assignRivers moves (fromWorld world)

    g =
      Graph.emap (const $ int 1) .
      Graph.elfilter (== pid) $ g0

    (mine, (profit, path)) =
      fmap (List.maximumBy (comparing fst)) .
      List.maximumBy (comparing (List.maximumBy (comparing fst) . snd)) .
      with mines $ \mid -> (mid,) $
      with (Graph.nodes g) $ \node ->
        let
          nodes =
            Graph.sp (siteId mid) node g

          distance =
            case nodes of
              [] ->
                0
              xs ->
                length xs - 1

          value =
            Score $ distance * distance

        in
          (value, nodes)

  in
    Route mine profit . Unboxed.fromList . fmap SiteId $ path
