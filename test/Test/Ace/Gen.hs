{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Ace.Gen where

import           Ace.Data

import qualified Disorder.Corpus as Corpus

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           P


genSiteId :: Gen SiteId
genSiteId =
  SiteId <$> Gen.int (Range.linear 0 100)

genPunter :: Gen Punter
genPunter =
  Punter <$> Gen.element Corpus.muppets
