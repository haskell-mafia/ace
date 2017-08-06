{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Ace.Serial where

import           Ace.Data.Protocol
import           Ace.Serial

import           Data.Aeson.Types (parseEither)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           P

import           System.IO (IO)

import           Test.Ace.Gen

prop_siteId :: Property
prop_siteId =
  property $ do
    n <- forAll genSiteId
    tripping n fromSiteId (parseEither toSiteId)

prop_site :: Property
prop_site =
  property $ do
    n <- forAll genSiteId
    tripping n fromSite (parseEither toSite)

prop_sites :: Property
prop_sites =
  property $ do
    n <- forAll genSites
    tripping n fromSites (parseEither toSites)

prop_mines :: Property
prop_mines =
  property $ do
    n <- forAll genMines
    tripping n fromMines (parseEither toMines)

prop_river :: Property
prop_river =
  property $ do
    n <- forAll genRiver
    tripping n fromRiver (parseEither toRiver)

prop_rivers :: Property
prop_rivers =
  property $ do
    n <- forAll genRivers
    tripping n fromRivers (parseEither toRivers)

prop_punter :: Property
prop_punter =
  property $ do
    p <- forAll genPunter
    tripping p fromPunter (parseEither toPunter)

prop_punter_me :: Property
prop_punter_me =
  property $ do
    p <- forAll genPunter
    tripping p (fromMe fromPunter) (parseEither (toMe toPunter))

prop_punter_you :: Property
prop_punter_you =
  property $ do
    p <- forAll genPunter
    tripping p (fromYou fromPunter) (parseEither (toYou toPunter))

prop_punterId :: Property
prop_punterId =
  property $ do
    n <- forAll genPunterId
    tripping n fromPunterId (parseEither toPunterId)

prop_punterCount :: Property
prop_punterCount =
  property $ do
    n <- forAll genPunterCount
    tripping n fromPunterCount (parseEither toPunterCount)

prop_move :: Property
prop_move =
  property $ do
    n <- forAll genPunterMove
    tripping n fromMove (parseEither toMove)

prop_moves :: Property
prop_moves =
  property $ do
    n <- forAll $ Gen.list (Range.linear 0 1000) genPunterMove
    tripping n fromMoves (parseEither toMoves)

prop_world :: Property
prop_world =
  property $ do
    n <- forAll genWorld
    tripping n fromWorld (parseEither toWorld)

prop_setup :: Property
prop_setup =
  property $ do
    n <- forAll genSetup
    tripping n fromSetup (parseEither toSetup)

prop_score :: Property
prop_score =
  property $ do
    n <- forAll genScore
    tripping n fromScore (parseEither toScore)

prop_punter_scores :: Property
prop_punter_scores =
  property $ do
    n <- forAll genPunterScores
    tripping n fromPunterScores (parseEither toPunterScores)

prop_stop :: Property
prop_stop =
  property $ do
    n <- forAll $ genStop
    tripping n fromStop (parseEither toStop)

prop_state :: Property
prop_state =
  property $ do
    n <- forAll $ genState
    tripping n fromState (parseEither toState)

prop_setup_result :: Property
prop_setup_result =
  property $ do
    n <- forAll $ genSetupResult
    tripping n fromSetupResult (parseEither toSetupResult)

prop_move_result :: Property
prop_move_result =
  property $ do
    n <- forAll $ genMoveResult
    tripping n fromMoveResult (parseEither toMoveResult)

prop_settings :: Property
prop_settings =
  property $ do
    n <- forAll genConfig
    tripping n fromConfig (parseEither toConfig)

prop_examples :: Property
prop_examples =
  property $ do
    asWith (toMe toPunter) "{\"me\":\"Alice\"}" === Right (Punter "Alice")
    asWith (toYou toPunter) "{\"you\":\"Alice\"}" === Right (Punter "Alice")

tests :: IO Bool
tests =
  checkParallel $$(discover)
