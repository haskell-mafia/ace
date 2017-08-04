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
    , Claim <$> genPunterId <*> genRiver
    ]

-- FIX this should be more realistic, just being used for serialisation at the moment
genWorld :: Gen World
genWorld =
  World <$> genSites <*> genMines <*> genRivers

-- FIX this should be more realistic, just being used for serialisation at the moment
genSetup :: Gen Setup
genSetup =
  Setup <$> genPunterId <*> genPunterCount <*> genWorld

genScore :: Gen Score
genScore =
  Score <$> genPunterId <*> Gen.int (Range.linear 0 100)

genScores :: Gen [Score]
genScores =
  Gen.list (Range.linear 1 100) genScore

-- FIX this should be more realistic, just being used for serialisation at the moment
genStop :: Gen Stop
genStop =
  Stop
    <$> Gen.list (Range.linear 1 100) genMove
    <*> genScores

genState :: Gen State
genState =
  State <$> genPunterId

genSetupResult :: Gen SetupResult
genSetupResult =
  SetupResult <$> genPunterId <*> genState

genMoveResult :: Gen MoveResult
genMoveResult =
  MoveResult <$> genMove <*> genState
