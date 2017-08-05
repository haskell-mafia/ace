{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ace.Robot.Random (
    random
  ) where

import           Ace.Data
import           Ace.Serial

import qualified Data.Vector.Unboxed as Unboxed

import           P

import           System.IO (IO)
import           System.Random (randomRIO)


random :: Robot [Move]
random =
  Robot "random" init move fromMoves toMoves

init :: Setup -> IO (Initialisation [Move])
init _ =
  pure $ Initialisation [] []

move :: Gameplay -> State [Move] -> IO (RobotMove [Move])
move g s = do
  let
    previousMoves =
      gameplay g <> stateData s

    foo =
      catMaybes . with previousMoves $ \x ->
        case x of
          Pass _ ->
            Nothing

          Claim _ r ->
            Just r

    rivers =
      Unboxed.filter (\r -> not $ r `elem` foo) $ worldRivers (stateWorld s)

    n =
      Unboxed.length rivers

  ix <- randomRIO (0, n - 1)

  case rivers Unboxed.!? ix of
    Nothing ->
      pure $ RobotPass previousMoves
    Just river ->
      pure $ RobotClaim previousMoves river
