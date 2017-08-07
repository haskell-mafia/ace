{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Ace.Gen where

import           Ace.Data.Config
import           Ace.Data.Core
import           Ace.Data.Future
import           Ace.Data.Protocol

import           Data.Aeson (ToJSON (..))
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

genPositions :: Int -> Gen (Unboxed.Vector Position)
genPositions n =
  fmap Unboxed.fromList $
    Gen.list (Range.singleton n) (Position <$> (Gen.double $ Range.linearFrac 0 100) <*> (Gen.double $ Range.linearFrac 0 100))

genMines :: Gen (Unboxed.Vector SiteId)
genMines = do
  n <- Gen.int (Range.linear 5 100)
  pure $ Unboxed.fromList $ SiteId <$> [0 .. n]

genRiver :: Gen River
genRiver =
  makeRiver <$> genSiteId <*> genSiteId

genRivers :: Gen (Unboxed.Vector River)
genRivers =
  Unboxed.fromList <$> Gen.list (Range.linear 5 100) genRiver

genFuture :: Gen Future
genFuture =
  Future <$> genSiteId <*> genSiteId

genFutures :: Gen [Future]
genFutures =
  Gen.list (Range.linear 5 100) genFuture

genPunter :: Gen Punter
genPunter =
  Punter <$> Gen.element Corpus.muppets

genPunterId :: Gen PunterId
genPunterId =
  PunterId <$> Gen.int (Range.linear 0 10)

genPunterCount :: Gen PunterCount
genPunterCount =
  PunterCount <$> Gen.int (Range.linear 2 10)

genPunterMove :: Gen PunterMove
genPunterMove =
  PunterMove <$> genPunterId <*> genMove

genMove :: Gen Move
genMove =
  Gen.choice [
      pure Pass
    , Claim <$> genRiver
    , Splurge <$> ((Route . Unboxed.fromList) <$> Gen.list (Range.linear 3 10) genSiteId)
    , Option <$> genRiver
    ]

-- FIX this should be more realistic, just being used for serialisation at the moment
genWorld :: Gen World
genWorld = do
  s <- genSites
  World <$> pure s <*> Gen.maybe (genPositions (Unboxed.length s)) <*> genMines <*> genRivers

-- FIX this should be more realistic, just being used for serialisation at the moment
genSetup :: Gen Setup
genSetup =
  Setup <$> genPunterId <*> genPunterCount <*> genWorld <*> genConfig

genScore :: Gen Score
genScore =
  Score <$> Gen.int (Range.linear 0 100)

genPunterScore :: Gen PunterScore
genPunterScore =
  PunterScore <$> genPunterId <*> genScore

genPunterScores :: Gen [PunterScore]
genPunterScores =
  Gen.list (Range.linear 1 100) genPunterScore

-- FIX this should be more realistic, just being used for serialisation at the moment
genStop :: Gen Stop
genStop =
  Stop
    <$> Gen.list (Range.linear 1 100) genPunterMove
    <*> genPunterScores
    <*> Gen.maybe genState

genState :: Gen State
genState =
  State <$> genPunterId <*> fmap toJSON (Gen.text (Range.linear 0 100) Gen.ascii)

genSetupResult :: Gen SetupResult
genSetupResult =
  SetupResult <$> genPunterId <*> genFutures <*> genState

genMoveResult :: Gen MoveResult
genMoveResult =
  MoveResult <$> genPunterMove <*> genState

genFutureFlag :: Gen FutureFlag
genFutureFlag =
  Gen.element [minBound .. maxBound]

genSplurgeFlag :: Gen SplurgeFlag
genSplurgeFlag =
  Gen.element [minBound .. maxBound]

genOptionFlag :: Gen OptionFlag
genOptionFlag =
  Gen.element [minBound .. maxBound]

genConfig :: Gen Config
genConfig =
  Config
    <$> genFutureFlag
    <*> genSplurgeFlag
    <*> genOptionFlag
