{-# LANGUAGE NoImplicitPrelude #-}
module Ace.Robot.Charles (
    charles
  ) where

import           Ace.Data

import           Data.Aeson (toJSON, parseJSON)

import           P

import           System.IO (IO)


charles :: Robot ()
charles =
  Robot init move toJSON parseJSON

init :: Setup -> IO ()
init _ =
  pure ()

move :: Gameplay -> State () -> IO (RobotMove ())
move _ _ =
  pure $ RobotPass ()
