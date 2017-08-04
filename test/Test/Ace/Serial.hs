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

prop_player :: Property
prop_player =
  property $ do
    p <- forAll genPlayer
    tripping p fromPlayer (parseEither toPlayer)

prop_player_me :: Property
prop_player_me =
  property $ do
    p <- forAll genPlayer
    tripping p (fromMe fromPlayer) (parseEither (toMe toPlayer))

prop_player_you :: Property
prop_player_you =
  property $ do
    p <- forAll genPlayer
    tripping p (fromYou fromPlayer) (parseEither (toYou toPlayer))

prop_examples :: Property
prop_examples =
  property $ do
    asWith (toMe toPlayer) "{\"me\":\"Alice\"}" === Right (Player "Alice")
    asWith (toYou toPlayer) "{\"you\":\"Alice\"}" === Right (Player "Alice")

tests :: IO Bool
tests =
  checkParallel $$(discover)
