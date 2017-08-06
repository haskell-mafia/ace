{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ace.Robot.Charles (
    charles
  ) where

import           Ace.Data.Core
import           Ace.Data.Future
import           Ace.Data.Robot

import           P

import           System.IO (IO)


charles :: Robot
charles =
  Robot "charles" init move

init :: PunterId -> PunterCount -> World -> FuturesFlag -> IO (Initialisation ())
init _ _ _ _ =
  pure $ Initialisation () []

move :: [PunterMove] -> () -> IO (RobotMove ())
move _ _ =
  pure $ RobotMove Pass ()
