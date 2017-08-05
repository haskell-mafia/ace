{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ace.Robot.Lannister (
    Lannister(..)
  , lannister
  ) where

import           Ace.Data
import           Ace.Serial

import qualified Data.Vector.Unboxed as Unboxed

import           P

import           System.IO (IO)
import           System.Random (randomRIO)


data Lannister =
    Cersei
  | Tyrion
    deriving (Eq, Show)

lannister :: Lannister -> Robot [Move]
lannister l =
  Robot (renderLannister l) init (move l) fromMoves toMoves

renderLannister :: Lannister -> Text
renderLannister l =
  case l of
    Cersei ->
      "witch"
    Tyrion ->
      "lion"

init :: Setup -> IO (Initialisation [Move])
init _ =
  pure $ Initialisation [] []

move :: Lannister -> Gameplay -> State [Move] -> IO (RobotMove [Move])
move l g s = do
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

  prefered <- case l of
    Cersei ->
      pure . head . sortOn riverSource $ Unboxed.toList preferedRivers

    Tyrion -> do
      jx <- randomRIO (0, m - 1)
      pure $ preferedRivers Unboxed.!? jx

  ix <- randomRIO (0, n - 1)

  case prefered of
    Just river ->
      pure $ RobotClaim previousMoves river
    Nothing ->
      case rivers Unboxed.!? ix of
        Nothing ->
          pure $ RobotPass previousMoves
        Just river ->
          pure $ RobotClaim previousMoves river
