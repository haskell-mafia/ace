{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Ace.Gen where

import           Ace.Data

import qualified Data.Vector.Unboxed as Unboxed
import qualified Disorder.Corpus as Corpus

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           P


genSiteId :: Gen SiteId
genSiteId =
  SiteId <$> Gen.int (Range.linear 0 100)

genSites :: Gen (Unboxed.Vector SiteId)
genSites = do
  n <- Gen.int (Range.linear 5 100)
  pure $ Unboxed.fromList $ SiteId <$> [0 .. n]

genMines :: Gen (Unboxed.Vector SiteId)
genMines = do
  n <- Gen.int (Range.linear 5 100)
  pure $ Unboxed.fromList $ SiteId <$> [0 .. n]

genRiver :: Gen River
genRiver =
  River <$> genSiteId <*> genSiteId

genRivers :: Gen (Unboxed.Vector River)
genRivers =
  Unboxed.fromList <$> Gen.list (Range.linear 5 100) genRiver

genPunter :: Gen Punter
genPunter =
  Punter <$> Gen.element Corpus.muppets

genPunterId :: Gen PunterId
genPunterId =
  PunterId <$> Gen.int (Range.linear 0 10)

genPunterCount :: Gen PunterCount
genPunterCount =
  PunterCount <$> Gen.int (Range.linear 2 10)

genMove :: Gen Move
genMove =
  Gen.choice [
      Pass <$> genPunterId
    , Claim <$> genPunterId <*> (Source <$> genSiteId) <*> (Target <$> genSiteId)
    ]

-- FIX this should be more realistic, just being used for serialisation at the moment
genWorld :: Gen World
genWorld =
  World <$> genSites <*> genMines <*> genRivers

-- FIX this should be more realistic, just being used for serialisation at the moment
genSetup :: Gen Setup
genSetup =
  Setup <$> genPunterId <*> genPunterCount <*> genWorld
