{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TupleSections #-}

module Ace.Random.Layout (
    Preset (..)
  , genWorld
  ) where

import           Ace.Data

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
genSites lower upper =
  Gen.filter
    (not . List.null)
    (List.nub <$>
       Gen.list
         (Range.linear lower upper)
         (SiteId <$> Gen.int (Range.linear 0 (2 * upper))))

genRiversBounded :: Int -> [SiteId] -> Gen [River]
genRiversBounded upper sites = do
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

genMines :: Int -> [SiteId] -> Gen [SiteId]
genMines percentage sites = do
  let
    n =
      length sites
    takes =
      round (fromIntegral n * fromIntegral percentage / (100 :: Double))
  wow <- Gen.subsequence sites
  -- if at first you don't succeed try only one more time
  if List.length wow < takes then do
    ohno <- Gen.subsequence (sites List.\\ wow)
    return . List.take takes $ ohno <> wow
  else
    return . List.take takes $ wow

genWorld :: Int -> Int -> Int -> Int -> Gen World
genWorld lower upper degree minePercentage = do
  sites <- genSites lower upper
  mines <- genMines minePercentage sites
  rivers <- genRiversBounded degree sites
  return $ World
    (Unboxed.fromList sites)
    (Unboxed.fromList mines)
    (Unboxed.fromList rivers)
