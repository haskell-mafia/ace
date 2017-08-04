{-# LANGUAGE NoImplicitPrelude #-}
module Ace.Serial (
    fromSiteId
  , toSiteId
  ) where

import           Ace.Data

import           Data.Aeson (Value (..), toJSON, parseJSON)
import           Data.Aeson.Types (Parser)

import           P


fromSiteId :: SiteId -> Value
fromSiteId =
  toJSON . siteId

toSiteId :: Value -> Parser SiteId
toSiteId v =
  SiteId <$> parseJSON v
