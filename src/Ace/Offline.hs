{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Ace.Offline (
    setup
  , play
  , score
  ) where

import           Ace.Data

import           P

setup :: Setup -> SetupResult ()
setup (Setup p c w) =
  SetupResult p (State p c w ())

play :: Gameplay -> State () -> MoveResult ()
play _ s =
  MoveResult (Pass $ PunterId 0) s

score :: Stop -> State () -> ()
score _ _ =
  ()
