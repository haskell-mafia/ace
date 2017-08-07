{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Ace.Analysis.River (
    State(..)
  , Owner(..)

  , init
  , update

  , claim
  , member
  , lookup
  , surrounds
  , filter
  , filterPunter
  , filterPunterOrUnclaimed
  , reachable
  , routes
  , routesFor
  , owners
  , unclaimed
  , optionable
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
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as Unboxed

import           GHC.Generics (Generic)

import           P hiding (filter)


data OwnedBy =
    Nobody
  | PrimaryHolder PunterId
  | PrimaryAndOptionHolders PunterId PunterId
    deriving (Eq, Ord, Show, Generic)

instance Binary OwnedBy

ownedBy :: PunterId -> OwnedBy -> Bool
ownedBy p o =
  case o of
    Nobody ->
      False
    PrimaryHolder c ->
      c == p
    PrimaryAndOptionHolders c option ->
      c == p || option == p

canOption :: PunterId -> OwnedBy -> Bool
canOption attempt =
  isJust . takeHolder attempt

takeHolder :: PunterId -> OwnedBy -> Maybe PunterId
takeHolder attempt o =
  case o of
    Nobody ->
      Nothing
    PrimaryHolder p ->
      if p /= attempt then
        Just p
      else
        Nothing
    PrimaryAndOptionHolders _ _ ->
      Nothing

takePrimary :: OwnedBy -> Maybe PunterId
takePrimary o =
  case o of
    Nobody ->
      Nothing
    PrimaryHolder p ->
      Just p
    PrimaryAndOptionHolders p _ ->
      Just p

data Owner =
  Owner {
      ownerRiver :: !River
    , ownerPunter :: !OwnedBy
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
      (getSiteId (riverSource x), getSiteId (riverTarget x), Owner x Nobody)

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

-- fgl models directed graphs, so we need edges in both directions for most operations
riverEdges :: River -> [Graph.Edge]
riverEdges x = [
    (getSiteId (riverSource x), getSiteId (riverTarget x))
  , (getSiteId (riverTarget x), getSiteId (riverSource x))
  ]

-- the canonical river edge, only use this for lookups
riverEdge :: River -> Graph.Edge
riverEdge x =
  (getSiteId (riverSource x), getSiteId (riverTarget x))

labelEdge :: a -> Graph.Edge -> Graph.LEdge a
labelEdge a (x, y) =
  (x, y, a)

-- | Returns 'True' if the river exists.
--
member :: River -> State -> Bool
member river state =
  Graph.hasEdge (stateOwners state) (riverEdge river)

lookup :: River -> State -> OwnedBy
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
        Nobody
      (Just (_, _, _, links), _) ->
        fromMaybe Nobody . listToMaybe . fmap (ownerPunter . fst) $
          List.filter ((== target) . snd) links

surrounds :: SiteId -> State -> Map SiteId (Maybe PunterId)
surrounds site state =
  let
    rivers =
      stateOwners state
  in
    case Graph.match (getSiteId site) rivers of
      (Nothing, _) ->
        Map.empty
      (Just (_, _, _, links), _) ->
        Map.fromList . with links $ \(owner, node) ->
          (SiteId node, takePrimary $ ownerPunter owner)

-- NOTE this doesn't do anything if the claim isn't valid
claim :: PunterBid -> State -> State
claim (PunterBid bid punter river) state =
  let
    label old =
      case takeHolder punter old of
        Nothing ->
          if bid == BidSplurge || bid == BidClaim then
            labelEdge $ Owner river (PrimaryHolder punter)
          else
            labelEdge $ Owner river old
        Just current ->
          if bid == BidSplurge || bid == BidOption then
            labelEdge $ Owner river (PrimaryAndOptionHolders current punter)
          else
            labelEdge $ Owner river old

    edges =
      riverEdges river

  in
    state {
      stateOwners =
        Graph.insEdges (fmap (label (lookup river state)) edges) $
        Graph.delEdges edges (stateOwners state)
    }

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
  filter (ownedBy punter . ownerPunter)

filterPunterOrUnclaimed :: PunterId -> State -> State
filterPunterOrUnclaimed punter =
  filter ((\owner -> ownedBy punter owner || owner == Nobody) . ownerPunter)

fromLRTree :: Graph.LRTree a -> Map SiteId Route
fromLRTree tree =
  Map.fromList .
  flip mapMaybe tree $ \case
    Graph.LP [] ->
      Nothing
    Graph.LP (route@((hd, _) : _)) -> do
      xs <- makeRoute . Unboxed.fromList . fmap (SiteId . fst) $ reverse route
      pure (SiteId hd, xs)

routesFor :: MineId -> State -> Map SiteId Route
routesFor mine =
  fromLRTree .
  Graph.spTree (getSiteId $ getMineId mine) .
  stateWeighted

routes :: Set MineId -> State -> Map Journey Route
routes mines state =
  Map.unions .
  with (Set.toList mines) $ \mine ->
    Map.mapKeysMonotonic (Journey mine) $ routesFor mine state

owners :: State -> Map River OwnedBy
owners =
  Map.fromList .
  fmap (\x -> (ownerRiver x, ownerPunter x)) .
  fmap Graph.edgeLabel .
  Graph.labEdges .
  stateOwners

unclaimed :: State -> Set River
unclaimed =
  Map.keysSet .
  owners .
  filter ((== Nobody) . ownerPunter)

optionable :: PunterId -> State -> Set River
optionable p =
  Map.keysSet .
  owners .
  filter (canOption p . ownerPunter)
