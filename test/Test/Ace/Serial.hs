{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Ace.Serial where

import           Ace.Data
import           Ace.Serial

import           Data.Aeson.Types (parseEither)

import           Hedgehog

import           P

import           System.IO (IO)

import           Test.Ace.Gen

import           X.Data.Aeson (asWith)

prop_siteId :: Property
prop_siteId =
  property $ do
    n <- forAll genSiteId
    tripping n fromSiteId (parseEither toSiteId)

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
    n <- forAll genMove
    tripping n fromMove (parseEither toMove)

prop_examples :: Property
prop_examples =
  property $ do
    asWith (toMe toPunter) "{\"me\":\"Alice\"}" === Right (Punter "Alice")
    asWith (toYou toPunter) "{\"you\":\"Alice\"}" === Right (Punter "Alice")

tests :: IO Bool
tests =
  checkParallel $$(discover)
