{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Ace.Serial where

import           Ace.Serial

import           Data.Aeson.Types (parseEither)

import           Hedgehog

import           P

import           System.IO (IO)

import           Test.Ace.Gen

prop_siteId :: Property
prop_siteId =
  property $ do
    n <- forAll genSiteId
    tripping n fromSiteId (parseEither toSiteId)

tests :: IO Bool
tests =
  checkParallel $$(discover)
