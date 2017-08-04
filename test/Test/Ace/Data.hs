{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Ace.Data where

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           P

import           System.IO (IO)


prop_example :: Property
prop_example =
  property $ do
    n <- forAll $ Gen.int (Range.linear 0 100)
    n === n

tests :: IO Bool
tests =
  checkParallel $$(discover)
