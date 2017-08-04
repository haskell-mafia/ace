{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ace.Serial (
    fromSiteId
  , toSiteId
  , fromPunter
  , toPunter
  , fromMe
  , toMe
  , fromYou
  , toYou
  , fromPunterId
  , toPunterId
  , fromPunterCount
  , toPunterCount
  , fromMove
  , toMove
  ) where

import           Ace.Data

import           Data.Aeson (Value (..), toJSON, parseJSON)
import           Data.Aeson (object, (.=), (.:), withObject)
import           Data.Aeson.Types (Parser)

import           P


fromSiteId :: SiteId -> Value
fromSiteId =
  toJSON . siteId

toSiteId :: Value -> Parser SiteId
toSiteId v =
  SiteId <$> parseJSON v

fromPunter :: Punter -> Value
fromPunter =
  toJSON . renderPunter

toPunter :: Value -> Parser Punter
toPunter v =
  Punter <$> parseJSON v

fromMe :: (a -> Value) -> a -> Value
fromMe f a =
  toJSON . object $ ["me" .= f a]

toMe :: (Value -> Parser a) -> Value -> Parser a
toMe p =
  withObject "me" $ \o ->
    o .: "me" >>= p

fromYou :: (a -> Value) -> a -> Value
fromYou f a =
  toJSON . object $ ["you" .= f a]

toYou :: (Value -> Parser a) -> Value -> Parser a
toYou p =
  withObject "you" $ \o ->
    o .: "you" >>= p

fromPunterId :: PunterId -> Value
fromPunterId =
  toJSON . punterId

toPunterId :: Value -> Parser PunterId
toPunterId v =
  PunterId <$> parseJSON v

fromPunterCount :: PunterCount -> Value
fromPunterCount =
  toJSON . punterCount

toPunterCount :: Value -> Parser PunterCount
toPunterCount v =
  PunterCount <$> parseJSON v

fromMove :: Move -> Value
fromMove m =
  case m of
    Pass p ->
      object [
          "pass" .= object [
               "punter" .= fromPunterId p
             ]
        ]
    Claim p (Source s) (Target t) ->
      object [
          "claim" .= object [
               "punter" .= fromPunterId p
             , "source" .= fromSiteId s
             , "target" .= fromSiteId t
             ]
        ]

toMove :: Value -> Parser Move
toMove v =
  toClaim v <|> toPass v

toClaim :: Value -> Parser Move
toClaim =
  withObject "Move" $ \o ->
    o .: "claim" >>= (withObject "Claim" $ \c ->
      Claim
        <$> (c .: "punter" >>= toPunterId)
        <*> (c .: "source" >>= fmap Source . toSiteId)
        <*> (c .: "target" >>= fmap Target . toSiteId))

toPass :: Value -> Parser Move
toPass =
  withObject "Move" $ \o ->
    o .: "pass" >>= (withObject "Pass" $ \c ->
      Pass
        <$> (c .: "punter" >>= toPunterId))
