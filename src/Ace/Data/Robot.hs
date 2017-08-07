{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
module Ace.Data.Robot (
    Initialisation (..)

  , Robot(..)
  , RobotMove(..)
  , RobotName(..)
  , nameOf

  , RobotIdentifier(..)
  ) where

import           Ace.Data.Config
import           Ace.Data.Core
import           Ace.Data.Future
import           Ace.Data.Protocol (Punter)

import           Data.Binary (Binary)

import           GHC.Generics (Generic)

import           P

import           System.IO


data RobotMove a =
  RobotMove {
      robotMoveValue :: !(Maybe Move)
    , robotMoveState :: !a
    } deriving (Eq, Ord, Show, Functor, Generic)


data Initialisation a =
  Initialisation {
      initialisationState :: !a
    , initialisationFutures :: [Future]
    } deriving (Eq, Show, Functor, Generic)

instance Monoid a => Monoid (Initialisation a) where
  mempty =
    Initialisation mempty []
  mappend (Initialisation x a) (Initialisation y b) =
    Initialisation (x <> y) (a <> b)

data Robot =
  forall a. (Binary a, Show a) =>
    Robot {
        robotLabel :: Text
      , robotInit :: PunterId -> PunterCount -> World -> Config -> IO (Initialisation a)
      , robotMove :: [PunterMove] -> a -> IO (RobotMove a)
      }

newtype RobotName =
  RobotName {
      robotName :: Text
    } deriving (Eq, Ord, Show, Generic)

nameOf :: Robot -> RobotName
nameOf robot =
  case robot of
    Robot name _ _ ->
      RobotName name

data RobotIdentifier =
  RobotIdentifier {
      identifierName:: !RobotName
    , identifierPunter :: !Punter
    } deriving (Eq, Ord, Show, Generic)
