{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Ace.Data.Web where

import           Ace.Data.Web

import           Control.Monad.IO.Class

import           Hedgehog

import           P

import           System.IO (IO)

prop_generate :: Property
prop_generate =
  property $ do
    a <- liftIO $ generateNewId
    b <- liftIO $ generateNewId
    assert . not $ a == b

tests :: IO Bool
tests =
  checkSequential $$(discover)
