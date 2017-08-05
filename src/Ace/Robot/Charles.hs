{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ace.Robot.Charles (
    charles
  ) where

import           Ace.Data

import           Data.Aeson (toJSON, parseJSON)

import           P

import           System.IO (IO)


charles :: Robot ()
charles =
  Robot "charles" init move toJSON parseJSON

init :: Setup -> IO (Initialisation ())
init _ =
  pure $ Initialisation () []

move :: Gameplay -> State () -> IO (RobotMove ())
move _ _ =
  pure $ RobotPass ()
