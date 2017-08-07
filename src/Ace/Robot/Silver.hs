{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ace.Robot.Silver (
    silver
  ) where

import           Ace.Data.Analysis
import           Ace.Data.Config
import           Ace.Data.Core
import           Ace.Data.Robot

import           Data.Binary (Binary)
import qualified Data.Graph.Inductive.Basic as Graph
import qualified Data.Graph.Inductive.Graph as Graph
import qualified Data.Graph.Inductive.Internal.RootPath as Graph
import           Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Graph.Inductive.Query.SP as Graph
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (isJust)
import qualified Data.Vector.Unboxed as Unboxed

import           GHC.Generics (Generic)

import           P

import           System.IO (IO)


data Silver =
  Silver {
      silverMoves :: [PunterMove]
    , silverScores :: Map (Graph.Node, Graph.Node) Int
    , silverWorld :: World
    , silverPunter :: PunterId
    } deriving (Eq, Ord, Show, Generic)

instance Binary Silver where

silver :: Robot
silver =
  Robot "silver" init move

fromWorld :: World -> Gr SiteId River
fromWorld world =
  let
    nodes =
      fmap (\x -> (siteId x, x)) . Unboxed.toList $ worldSites world

    edges =
      fmap (\r -> (siteId (riverSource r), siteId (riverTarget r), r)) . Unboxed.toList $ worldRivers world
  in
    Graph.undir $ Graph.mkGraph nodes edges

assignRivers :: [PunterMove] -> Gr SiteId River -> Gr SiteId (Maybe PunterId)
assignRivers moves g =
  let
    kv (PunterClaim v k) =
      (k, v)

    pids =
      Map.fromList . fmap kv $ mapMaybe takeClaim moves
  in
    Graph.emap (flip Map.lookup pids) g

init :: PunterId -> PunterCount -> World -> Config -> IO (Initialisation Silver)
init punter _ world _ =
  let
    graph =
      Graph.emap (const (1 :: Int)) $ fromWorld world

    kvs =
      Map.fromList . concat .
      with (Unboxed.toList $ worldMines world) $ \mid ->
        let
          tree = Graph.spTree (siteId mid) graph
        in
          flip mapMaybe (Graph.nodes graph) $ \node ->
            case Graph.getLPathNodes node tree of
              [] ->
                Nothing
              xs ->
                let
                  n =
                    length xs - 1
                in
                  Just ((siteId mid, node), n * n)
  in
    pure $ Initialisation (Silver [] kvs world punter) []

scorePath :: Graph.Path -> Gr SiteId (Maybe PunterId) -> Int
scorePath nodes graph0 =
  let
    graph =
      Graph.elfilter (not . isJust) graph0
  in
    sum . with (pairwise nodes) $ \edge ->
      if Graph.hasEdge graph edge then
        0
      else
        1

pairwise :: Graph.Path -> [(Graph.Node, Graph.Node)]
pairwise = \case
  [] ->
    []
  xs ->
    List.zip xs (List.tail xs)

fromPath :: Graph.Path -> Gr SiteId (Maybe PunterId) -> Maybe River
fromPath nodes graph0 =
  let
    graph =
      Graph.elfilter (not . isJust) graph0
  in
    head . flip mapMaybe (pairwise nodes) $ \edge@(n0, n1) ->
      if Graph.hasEdge graph edge then
        Just $ makeRiver (SiteId n0) (SiteId n1)
      else
        Nothing

move :: [PunterMove] -> Silver -> IO (RobotMove Silver)
move g s = do
  let
    pid =
      silverPunter s

    mines =
      Unboxed.toList . worldMines $ silverWorld s

    scores =
      silverScores s

    previousMoves =
      g <> silverMoves s

    graph0 =
      fromWorld $ silverWorld s

    graph1 =
      Graph.elfilter (\x -> x == Just pid || x == Nothing) $
      assignRivers previousMoves graph0

    graph1_weighted =
      Graph.emap (const (1 :: Int)) graph1

    fromTuple (n, m, x) =
      fmap (n, m,) $ fromPath x graph1

    updated =
      s { silverMoves = previousMoves }

    fromPaths xs =
      case xs of
        [] ->
          pure $ RobotMove Pass updated

        (_, _, x) : _ ->
          pure $ RobotMove (Claim x) updated

    everyMove =
      catMaybes . with previousMoves $ \x ->
        case x of
          PunterMove _ Pass ->
            Nothing

          PunterMove _ (Claim r) ->
            Just r

          -- FIX add support for splurge
          PunterMove _ (Splurge _) ->
            Nothing

          PunterMove _ (Option r) ->
            Just r


    everyMove1 =
      concat . with previousMoves $ \x ->
        case x of
          PunterMove _ Pass ->
            []

          PunterMove _ (Claim r) ->
            [riverSource r, riverTarget r]

          PunterMove _ (Splurge r) ->
            Unboxed.toList . getRoute $ r

          PunterMove _ (Option r) ->
            [riverSource r, riverTarget r]


    ours =
      concat . with previousMoves $ \x ->
        case x of
          PunterMove _ Pass ->
            []

          PunterMove p (Claim r) ->
            if p == pid then
              [riverSource r, riverTarget r]
            else
              []

          PunterMove p (Splurge r) ->
            if p == pid then
              Unboxed.toList . getRoute $ r
            else
              []

          PunterMove p (Option r) ->
            if p == pid then
              [riverSource r, riverTarget r]
            else
              []

    rivers =
      Unboxed.filter (\r -> not $ r `elem` everyMove) $ worldRivers (silverWorld s)

    -- Prefer mines we haven't visited and have two other rivers taken by other players
    mines1 =
      Unboxed.filter (\r -> (length (filter (== r) everyMove1) >= 2) && (not $ r `elem` ours)) $ worldMines (silverWorld s)

    preferedRivers =
      Unboxed.filter (\r -> riverSource r `Unboxed.elem` mines1 || riverTarget r `Unboxed.elem` mines1) $ rivers

    prefered =
      head . sortOn riverSource $ Unboxed.toList preferedRivers

  case prefered of
    Just river ->
      pure $ RobotMove (Claim river) updated
    Nothing ->
      fromPaths .
      mapMaybe fromTuple .
      sortOn (\(x, y, _) -> Down (x, y)) .
      concat .
      with mines $ \mid ->
      let
        tree = Graph.spTree (siteId mid) graph1_weighted
      in
        with (Graph.nodes graph1) $ \node ->
          let
            path =
              Graph.getLPathNodes node tree

            nodeScore =
              fromMaybe 0 $
                Map.lookup (siteId mid, node) scores
          in
            (nodeScore, scorePath path graph1, path)
