{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Ace.Random.Layout where

import Ace.Data

import qualified Data.List as List
import qualified Data.Vector.Unboxed as Unboxed

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import P


newtype Connected =
  Connected {
      connectedPercentage :: Int
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

genRivers :: [SiteId] -> Gen [River]
genRivers sites = do
  let
    complete =
      [River x y | x <- sites, y <- sites, x /= y]
  Gen.subsequence complete

genRiversWith :: Connected -> [SiteId] -> Gen [River]
genRiversWith c sites = do
  let
    complete =
      [River x y | x <- sites, y <- sites, x /= y]
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

genMines :: [SiteId] -> Gen [SiteId]
genMines =
  Gen.filter (not . List.null) . Gen.subsequence

genWorld :: Gen World
genWorld = do
  sites <- genSites
  mines <- genMines sites
  rivers <- genRivers sites
  return $ World
    (Unboxed.fromList sites)
    (Unboxed.fromList mines)
    (Unboxed.fromList rivers)

genWorldWith :: Int -> Gen World
genWorldWith c = do
  sites <- genSites
  mines <- genMines sites
  rivers <- genRiversWith (Connected c) sites
  return $ World
    (Unboxed.fromList sites)
    (Unboxed.fromList mines)
    (Unboxed.fromList rivers)
