{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ace.Robot.Ibis (
    ibis
  ) where

import           Ace.Data
import           Ace.Serial
import qualified Ace.Robot.Charles as Charles
import qualified Ace.Robot.Lannister as Lannister
import qualified Ace.Robot.Random as Random

import           P

import           System.IO (IO)
import           System.Random (randomRIO)


ibis :: Robot [Move]
ibis =
  Robot "ibis" init move fromMoves toMoves

init :: Setup -> IO (Initialisation [Move])
init _ =
  pure $ Initialisation [] []

move :: Gameplay -> State [Move] -> IO (RobotMove [Move])
move g s = do
  ix <- randomRIO (0, 2)

  let
    previousMoves =
      gameplay g <> stateData s

  case ix :: Int of
    0 -> do
      fmap (previousMoves <$) $ (robotMove Charles.charles) g (() <$ s)
    1 ->
      (robotMove Lannister.lannister) g s
    2 ->
      (robotMove Random.random) g s
    _ ->
      (robotMove Random.random) g s
