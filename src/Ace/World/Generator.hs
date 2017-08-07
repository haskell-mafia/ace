{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TupleSections #-}
module Ace.World.Generator (
    Preset (..)
  , genWorld
  , genWorld_
  ) where

import           Ace.Data.Core

import qualified Control.Monad.Trans.State.Strict as State

import qualified Data.List as List
import qualified Data.Vector as Boxed
import qualified Data.Vector.Unboxed as Unboxed

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           P


data Preset =
  Preset {
      worldSitesLowerBound :: !Int
    , worldSitesUpperBound :: !Int
    , worldMinePercentage :: !Int
    , worldDegreeUpperBound :: !Int
    } deriving (Eq, Ord, Show)

genSites :: Int -> Int -> Gen [SiteId]
genSites lower upper = do
  n <- Gen.int $ Range.linear lower upper
  return . fmap SiteId $ [0..n]

genRiversBounded :: Int -> [SiteId] -> Gen [River]
genRiversBounded upper sites = do
  let
    pick = do
      (rivers, counts) <- State.get
      if List.null rivers then
        return []
      else do
        river <- Gen.element rivers
        let
          source =
            riverSource river
          sourceCount =
            counts Boxed.! getSiteId source
          target =
            riverTarget river
          targetCount =
            counts Boxed.! getSiteId target
          river' =
            makeRiver target source
        if sourceCount >= upper || targetCount >= upper then do
            State.put (rivers List.\\ [river, river'], counts)
            pick
        else do
          dice <- Gen.int $ Range.constant 0 10
          if dice < 1 then do
            let
              counts' =
                Boxed.unsafeUpd counts
                  [ (getSiteId source, sourceCount + 1)
                  , (getSiteId target, targetCount + 1) ]
            State.put (rivers List.\\ [river, river'], counts')
            xs <- pick
            return (river : xs)
          else do
            State.put (rivers List.\\ [river, river'], counts)
            pick

    line =
      [makeRiver x (SiteId (getSiteId x - 1)) | x <- drop 1 sites]

    leftovers =
      [makeRiver x y | x <- sites, y <- sites, x /= y, getSiteId x /= getSiteId y + 1]

  rivers <- Gen.shuffle leftovers

  let
    counts =
      Boxed.unsafeUpd
        (Boxed.replicate (List.length sites) 2)
        [(0, 1), (length sites - 1, 1)]

  (chosen, _) <- State.runStateT pick $ (rivers, counts)

  return $ line <> chosen

genMines :: Int -> [SiteId] -> Gen [MineId]
genMines percentage sites = do
  let
    n =
      length sites
    takes =
      round (fromIntegral n * fromIntegral percentage / (100 :: Double))
    mines =
      fmap MineId sites
  wow <- Gen.subsequence =<< Gen.shuffle mines
  -- if at first you don't succeed try only one more time
  if List.length wow < takes then do
    ohno <- Gen.subsequence (mines List.\\ wow)
    return . List.take takes $ ohno <> wow
  else
    return . List.take takes $ wow

genWorld_ :: Int -> Gen World
genWorld_ upper =
  genWorld (upper `div` 2) upper 3 10

genWorld :: Int -> Int -> Int -> Int -> Gen World
genWorld lower upper degree minePercentage = do
  sites <- genSites lower upper
  mines <- genMines minePercentage sites
  rivers <- genRiversBounded degree sites
  return $ World
    (Unboxed.fromList sites)
    Nothing
    (Unboxed.fromList mines)
    (Unboxed.fromList rivers)
