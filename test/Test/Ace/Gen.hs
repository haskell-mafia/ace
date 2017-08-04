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
