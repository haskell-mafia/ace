{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
module Ace.Robot.Silver (
    silver
  ) where

import           Ace.Data
import           Ace.Score
import qualified Ace.Serial as Serial

import qualified Data.Graph.Inductive.Basic as Graph
import qualified Data.Graph.Inductive.Graph as Graph
import           Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Graph.Inductive.Query.SP as Graph
import qualified Data.List as List
import           Data.Maybe (isJust)
import qualified Data.Vector.Unboxed as Unboxed

import           P

import           System.IO (IO)


silver :: Robot [Move]
silver =
  Robot "silver" init move Serial.fromMoves Serial.toMoves

init :: Setup -> IO (Initialisation [Move])
init _ =
  pure $ Initialisation [] []

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

move :: Gameplay -> State [Move] -> IO (RobotMove [Move])
move g s =
  let
    pid =
      statePunter s

    mines =
      Unboxed.toList . worldMines $ stateWorld s

    previousMoves =
      gameplay g <> stateData s

    graph0 =
      assignRivers previousMoves . fromWorld $ stateWorld s

    graph0_weighted =
      Graph.emap (const (1 :: Int)) graph0

    graph1 =
      Graph.elfilter (\x -> x == Just pid || x == Nothing) graph0

    graph1_weighted =
      Graph.emap (const (1 :: Int)) graph1

    fromTuple (n, m, x) =
      fmap (n, m,) $ fromPath x graph1

    fromPaths xs =
      case xs of
        [] ->
          pure $ RobotPass previousMoves

        (_, _, x) : _ ->
          pure $ RobotClaim previousMoves x
  in
    fromPaths .
    mapMaybe fromTuple .
    sortOn (\(x, y, _) -> Down (x, y)) .
    concat .
    with mines $ \mid ->
    with (Graph.nodes graph1) $ \node ->
      let
        path =
          Graph.sp (siteId mid) node graph1_weighted

        distance =
          case Graph.sp (siteId mid) node graph0_weighted of
            [] ->
              0
            xs ->
              length xs - 1
      in
        (distance * distance, scorePath path graph1, path)
