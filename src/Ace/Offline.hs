{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Ace.Offline (
    MoveResult(..)

  , setup
  , play
  , score
  ) where

import           Ace.Data

import           P

data MoveResult =
  MoveResult {
      moveResultMove :: !Move
    , moveResultState :: !State
    } deriving (Eq, Ord, Show)

setup :: Setup -> State
setup _ =
  State

play :: Gameplay -> State -> MoveResult
play _ s =
  MoveResult (Pass $ PunterId 0) s

score :: Scoring -> ()
score _ =
  ()
