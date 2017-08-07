{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Ace.Analysis.River (
    State(..)
  , Owner(..)

  , init
  , update

  , claim
  , lookup
  , filter
  , filterPunter
  , reachable
  , routes
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
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as Unboxed

import           GHC.Generics (Generic)

import           P hiding (filter)


data Owner =
  Owner {
      ownerRiver :: !River
    , ownerPunter :: !(Maybe PunterId)
    } deriving (Eq, Ord, Show, Generic)

instance Binary Owner

data State =
  State {
      stateOwners :: !(Gr SiteId Owner)
    } deriving (Eq, Show, Generic)

instance Binary State

-- FIX lens
withRiverOwners :: (Gr SiteId Owner -> Gr SiteId Owner) -> State -> State
withRiverOwners f state =
  state { stateOwners = f (stateOwners state) }

stateWeighted :: State -> Gr SiteId Int
stateWeighted =
  Graph.emap (const 1) .
  stateOwners

initRivers :: Unboxed.Vector SiteId -> Unboxed.Vector River -> Gr SiteId Owner
initRivers sites rivers =
  let
    node x =
      (getSiteId x, x)

    nodes =
      fmap node $ Unboxed.toList sites

    edge x =
      (getSiteId (riverSource x), getSiteId (riverTarget x), Owner x Nothing)

    edges =
      fmap edge $ Unboxed.toList rivers
  in
    Graph.undir $ Graph.mkGraph nodes edges

init :: World -> State
init world =
  let
    rivers =
      initRivers (worldSites world) (worldRivers world)
  in
    State rivers

-- fgl models directed graphs, so we need edges in both directions
riverEdges :: River -> [Graph.Edge]
riverEdges x = [
    (getSiteId (riverSource x), getSiteId (riverTarget x))
  , (getSiteId (riverTarget x), getSiteId (riverSource x))
  ]

labelEdge :: a -> Graph.Edge -> Graph.LEdge a
labelEdge a (x, y) =
  (x, y, a)

lookup :: River -> State -> Maybe PunterId
lookup river state =
  let
    rivers =
      stateOwners state

    source =
      siteNode $ riverSource river

    target =
      siteNode $ riverTarget river
  in
    case Graph.match source rivers of
      (Nothing, _) ->
        Nothing
      (Just (_, _, _, links), _) ->
        join . listToMaybe . fmap (ownerPunter . fst) $
          List.filter ((== target) . snd) links

claim :: PunterClaim -> State -> State
claim (PunterClaim punter river) state =
  let
    label =
      labelEdge $ Owner river (Just punter)

    edges =
      riverEdges river
  in
    case lookup river state of
      Nothing ->
        state {
          stateOwners =
            Graph.insEdges (fmap label edges) $
            Graph.delEdges edges (stateOwners state)
        }
      Just _owner ->
        state

update :: [PunterMove] -> State -> State
update moves state0 =
  foldr claim state0 (concatMap takeClaims moves)

siteNode :: SiteId -> Graph.Node
siteNode =
  getSiteId

reachable :: SiteId -> State -> [SiteId]
reachable site =
  fmap SiteId . Graph.reachable (siteNode site) . stateOwners

filter :: (Owner -> Bool) -> State -> State
filter f =
  withRiverOwners (Graph.elfilter f)

filterPunter :: PunterId -> State -> State
filterPunter punter =
  filter ((== Just punter) . ownerPunter)

fromLRTree :: Graph.LRTree a -> Map SiteId Route
fromLRTree tree =
  Map.fromList .
  flip mapMaybe tree $ \case
    Graph.LP [] ->
      Nothing
    Graph.LP (route@((hd, _) : _)) -> do
      xs <- makeRoute . Unboxed.fromList . fmap (SiteId . fst) $ reverse route
      pure (SiteId hd, xs)

routes :: MineId -> State -> Map SiteId Route
routes mine =
  fromLRTree .
  Graph.spTree (getSiteId $ getMineId mine) .
  stateWeighted
