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

  ) where

import           Ace.Data.Core
import           Ace.Data.Future

import           Data.Binary (Binary)

import           GHC.Generics (Generic)

import           P

import           System.IO


data RobotMove a =
  RobotMove {
      robotMoveValue :: !Move
    , robotMoveState :: !a
    } deriving (Eq, Ord, Show, Functor, Generic)


data Initialisation a =
  Initialisation {
      initialisationState :: !a
    , initialisationFutures :: [Future]
    } deriving (Eq, Show, Generic)

data Robot =
  forall a. (Binary a, Show a) =>
    Robot {
        robotLabel :: Text
      , robotInit :: PunterId -> PunterCount -> World -> FuturesFlag -> IO (Initialisation a)
      , robotMove :: [PunterMove] -> a -> IO (RobotMove a)
      }
