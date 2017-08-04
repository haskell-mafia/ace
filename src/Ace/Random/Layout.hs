{-# LANGUAGE NoImplicitPrelude #-}
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
      [River x y | x <- sites, y <- sites]

  Gen.subsequence complete

genMines :: [SiteId] -> Gen [SiteId]
genMines =
  Gen.filter (not . List.null) . Gen.subsequence

genWorld :: Gen World
genWorld = do
  sites <- genSites
  mines <- genMines sites
  rivers <- genRivers sites
  return $ World (Unboxed.fromList sites) (Unboxed.fromList mines) (Unboxed.fromList rivers)