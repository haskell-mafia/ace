{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Ace.Analysis.Score (
    ScoreState(..)
  , init
  , update
  , score
  ) where

import           Ace.Data.Analysis
import           Ace.Data.Binary ()
import           Ace.Data.Core

import           Data.Binary (Binary)
import qualified Data.Graph.Inductive.Basic as Graph
import qualified Data.Graph.Inductive.Graph as Graph
import           Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Graph.Inductive.Query.DFS as Graph
import qualified Data.Graph.Inductive.Query.SP as Graph
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as Unboxed

import           GHC.Generics (Generic)

import           P


data Owner =
  Owner {
      ownerRiver :: !River
    , ownerPunter :: !(Maybe PunterId)
    } deriving (Eq, Ord, Show, Generic)

instance Binary Owner

data ScoreState =
  ScoreState {
      statePunters :: !PunterCount
    , stateRivers :: !(Gr SiteId Owner)
    , stateMines :: !(Map SiteId (Map SiteId Route))
    , stateJourneys :: !(Map Journey Distance)
    } deriving (Eq, Show, Generic)

instance Binary ScoreState

-- fgl models directed graphs, so we need edges in both directions
riverEdges :: River -> [Graph.Edge]
riverEdges x = [
    (siteId (riverSource x), siteId (riverTarget x))
  , (siteId (riverTarget x), siteId (riverSource x))
  ]

labelEdge :: a -> Graph.Edge -> Graph.LEdge a
labelEdge a (x, y) =
  (x, y, a)

siteNode :: SiteId -> Graph.Node
siteNode =
  siteId

reachableSites :: SiteId -> Gr SiteId a -> [SiteId]
reachableSites site =
  fmap SiteId . Graph.reachable (siteNode site)

punterRivers :: PunterId -> ScoreState -> Gr SiteId River
punterRivers punter =
  Graph.emap ownerRiver .
  Graph.elfilter ((== Just punter) . ownerPunter) .
  stateRivers

punterJourneys :: PunterId -> ScoreState -> Map SiteId [Journey]
punterJourneys punter state =
  let
    rivers =
      punterRivers punter state

    takeJourneys mine _routes =
      fmap (Journey mine) $ reachableSites mine rivers
  in
    Map.mapWithKey takeJourneys (stateMines state)

applyClaim :: PunterClaim -> Gr SiteId Owner -> Gr SiteId Owner
applyClaim (PunterClaim punter river) =
  let
    label =
      labelEdge $ Owner river (Just punter)

    edges =
      riverEdges river
  in
    Graph.insEdges (fmap label edges) .
    Graph.delEdges edges

initRivers :: Unboxed.Vector SiteId -> Unboxed.Vector River -> Gr SiteId Owner
initRivers sites rivers =
  let
    node x =
      (siteId x, x)

    nodes =
      fmap node $ Unboxed.toList sites

    edge x =
      (siteId (riverSource x), siteId (riverTarget x), Owner x Nothing)

    edges =
      fmap edge $ Unboxed.toList rivers
  in
    Graph.undir $ Graph.mkGraph nodes edges

fromLRTree :: Graph.LRTree a -> Map SiteId Route
fromLRTree routes =
  Map.fromList .
  flip mapMaybe routes $ \case
    Graph.LP [] ->
      Nothing
    Graph.LP (route@((hd, _) : _)) -> do
      xs <- makeRoute . Unboxed.fromList . fmap (SiteId . fst) $ reverse route
      pure (SiteId hd, xs)

initMines :: Unboxed.Vector SiteId -> Gr SiteId a -> Map SiteId (Map SiteId Route)
initMines mines rivers0 =
  let
    rivers =
      Graph.emap (const (1 :: Int)) rivers0

    routes mine =
      fromLRTree $ Graph.spTree (siteId mine) rivers

    tree mine =
      (mine, routes mine)
  in
    Map.fromList . fmap tree $ Unboxed.toList mines

initJourneys :: Map SiteId (Map SiteId Route) -> Map Journey Distance
initJourneys mines =
  Map.fromList .
  concat .
  with (Map.toList mines) $ \(mine, routes) ->
  with (Map.toList routes) $ \(site, route) ->
    (Journey mine site, routeDistance route)

init :: World -> PunterCount -> ScoreState
init world punters =
  let
    rivers =
      initRivers (worldSites world) (worldRivers world)

    mines =
      initMines (worldMines world) rivers

    journeys =
      initJourneys mines
  in
    ScoreState punters rivers mines journeys

update :: [PunterMove] -> ScoreState -> ScoreState
update moves state =
  let
    claims =
      foldr applyClaim (stateRivers state) (mapMaybe takeClaim moves)
  in
    state { stateRivers = claims }

scoreJourney :: Journey -> ScoreState -> Score
scoreJourney journey state =
  fromMaybe 0 . fmap distanceScore . Map.lookup journey $ stateJourneys state

scoreJourneys :: Map SiteId [Journey] -> ScoreState -> Score
scoreJourneys journeys state =
  sum . fmap (flip scoreJourney state) . concat $ Map.elems journeys

score :: PunterId -> ScoreState -> Score
score punter state =
  scoreJourneys (punterJourneys punter state) state
