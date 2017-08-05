{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Ace.Offline (
    setup
  , play
  , score
  ) where

import           Ace.Data

import qualified Data.Text as Text

import           P

import           System.IO (IO)
import qualified System.IO as IO

setup :: Robot a -> Setup -> IO (SetupResult (State a))
setup r s@(Setup p c w settings) = do
  IO.hPutStrLn IO.stderr . Text.unpack $ "Running robot:" <> robotLabel r
  x <- robotInit r s
  pure $ SetupResult p (initialisationFutures x) (State p c w settings $ initialisationState x)

play :: Robot a -> Gameplay -> State a -> IO (MoveResult (State a))
play r g s = do
  m <- robotMove r g s
  pure $ fromRobotMove s m
