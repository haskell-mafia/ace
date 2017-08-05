{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ace.Robot.Lannister (
    lannister
  ) where

import           Ace.Data
import           Ace.Serial

import qualified Data.Vector.Unboxed as Unboxed

import           P

import           System.IO (IO)
import           System.Random (randomRIO)


lannister :: Robot [Move]
lannister =
  Robot "lion" init move fromMoves toMoves

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

    mines =
      worldMines (stateWorld s)

    preferedRivers =
      Unboxed.filter (\r -> riverSource r `Unboxed.elem` mines || riverTarget r `Unboxed.elem` mines) $ rivers

    n =
      Unboxed.length rivers

    m =
      Unboxed.length preferedRivers

  ix <- randomRIO (0, n - 1)

  jx <- randomRIO (0, m - 1)

  case preferedRivers Unboxed.!? jx of
    Just river ->
      pure $ RobotClaim previousMoves river
    Nothing ->
      case rivers Unboxed.!? ix of
        Nothing ->
          pure $ RobotPass previousMoves
        Just river ->
          pure $ RobotClaim previousMoves river
