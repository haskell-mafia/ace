{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TupleSections #-}

module Ace.Random.Layout where

import Ace.Data

import qualified Control.Monad.Trans.State.Strict as State

import qualified Data.List as List
import qualified Data.Vector as Boxed
import qualified Data.Vector.Unboxed as Unboxed

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import P


newtype Connected =
  Connected {
      connectedPercentage :: Int
    } deriving (Eq, Ord, Show)

newtype Degree =
  Degree {
      vertexDegreeUpperBound :: Int
    } deriving (Eq, Ord, Show)

genSites :: Gen [SiteId]
genSites =
  let
    n =
      128
  in
    Gen.filter
      (not . List.null)
      (List.nub <$> Gen.list (Range.linear 0 n) (SiteId <$> Gen.int (Range.linear 0 (2*n))))

nubRivers :: [River] -> [River]
nubRivers =
  let
    same (River x y) (River a b) =
      x == b && y == a
  in
    List.nubBy same

genRiversConnected :: Connected -> [SiteId] -> Gen [River]
genRiversConnected c sites = do
  let
    complete =
      nubRivers [River x y | x <- sites, y <- sites, x /= y]
    n =
      2 * length complete
    takes =
      round (fromIntegral n * (fromIntegral (connectedPercentage c) / (100 :: Double)))

  wow <- Gen.subsequence complete

  -- if at first you don't succeed try only one more time
  if List.length wow < takes then do
    ohno <- Gen.subsequence (complete List.\\ wow)
    return . List.take takes $ ohno <> wow
  else
    return . List.take takes $ wow

genRiversBounded :: Degree -> [SiteId] -> Gen [River]
genRiversBounded (Degree upper) sites = do
  let
    complete =
      [River x y | x <- sites, y <- sites, x /= y]

    pick = do
      (rivers, counts) <- State.get
      if List.null rivers then
        return []
      else do
        river <- Gen.element rivers
        let
          s =
            counts Boxed.! siteId (riverSource river)
          t =
            counts Boxed.! siteId (riverTarget river)
        if s > upper then do
          State.put (List.delete river rivers, counts)
          pick
        else do
          if t > upper then do
            State.put (List.delete river rivers, counts)
            pick
          else do
            let
              counts' =
                Boxed.unsafeUpd counts
                  [ (siteId . riverSource $ river, s + 1)
                  , (siteId . riverTarget $ river, t + 1) ]
            State.put (List.delete river rivers, counts')
            xs <- pick
            return (river : xs)

  rivers <- Gen.shuffle complete

  fmap fst . State.runStateT pick $
    (rivers, Boxed.replicate (List.maximum (fmap siteId sites) + 1) 0)

genMines :: [SiteId] -> Gen [SiteId]
genMines =
  Gen.filter (not . List.null) . Gen.subsequence

genWorldBounded :: Int -> Gen World
genWorldBounded upperBound = do
  sites <- genSites
  mines <- genMines sites
  rivers <- genRiversBounded (Degree upperBound) sites
  return $ world sites mines rivers

genWorldConnected :: Int -> Gen World
genWorldConnected c = do
  sites <- genSites
  mines <- genMines sites
  rivers <- genRiversConnected (Connected c) sites
  return $ world sites mines rivers

world :: [SiteId] -> [SiteId] -> [River] -> World
world sites mines rivers =
  World
    (Unboxed.fromList sites)
    (Unboxed.fromList mines)
    (Unboxed.fromList rivers)
