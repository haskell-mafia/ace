{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}

module Ace.Analysis.Data (
    OwnedBy (..)
  , Owner (..)
  , ownedBy
  , canOption
  , takeHolder
  ) where

import           Ace.Data.Core

import           GHC.Generics (Generic)

import           P

import           Data.Binary (Binary)


data OwnedBy =
    Nobody
  | PrimaryHolder PunterId
  | PrimaryAndOptionHolders PunterId PunterId
    deriving (Eq, Ord, Show, Generic)

instance Binary OwnedBy

ownedBy :: PunterId -> OwnedBy -> Bool
ownedBy p o =
  case o of
    Nobody ->
      False
    PrimaryHolder c ->
      c == p
    PrimaryAndOptionHolders c option ->
      c == p || option == p

canOption :: PunterId -> OwnedBy -> Bool
canOption attempt =
  isJust . takeHolder attempt

takeHolder :: PunterId -> OwnedBy -> Maybe PunterId
takeHolder attempt o =
  case o of
    Nobody ->
      Nothing
    PrimaryHolder p ->
      if p /= attempt then
        Just p
      else
        Nothing
    PrimaryAndOptionHolders _ _ ->
      Nothing

data Owner =
  Owner {
      ownerRiver :: !River
    , ownerPunter :: !OwnedBy
    } deriving (Eq, Ord, Show, Generic)

instance Binary Owner
