{-# LANGUAGE NoImplicitPrelude #-}
module Ace.Robot.Random (
    random
  ) where

import           Ace.Data

import           Data.Aeson (toJSON, parseJSON)
import qualified Data.Vector.Unboxed as Unboxed

import           P

import           System.IO (IO)
import           System.Random (randomRIO)


random :: Robot ()
random =
  Robot init move toJSON parseJSON

init :: Setup -> IO ()
init _ =
  pure ()

move :: Gameplay -> State () -> IO (RobotMove ())
move _ s = do
  let
    rivers =
      worldRivers (stateWorld s)

    n =
      Unboxed.length rivers

  ix <- randomRIO (0, n - 1)

  case rivers Unboxed.!? ix of
    Nothing ->
      pure $ RobotPass ()
    Just river ->
      pure $ RobotClaim () river
