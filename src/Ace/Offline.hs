{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Ace.Offline (
    setup
  , play
  , score
  ) where

import           Ace.Data

import           P

import           System.IO (IO)


setup :: Robot a -> Setup -> IO (SetupResult a)
setup r s@(Setup p c w settings) = do
  x <- robotInit r s
  pure $ SetupResult p (State p c w settings x)

play :: Robot a -> Gameplay -> State a -> IO (MoveResult a)
play r g s = do
  m <- robotMove r g s
  pure $ fromRobotMove s m
