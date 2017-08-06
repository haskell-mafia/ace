{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ace.Robot.Random (
    random
  ) where

import           Ace.Data.Config
import           Ace.Data.Core
import           Ace.Data.Robot

import           Data.Binary (Binary)
import qualified Data.Vector.Unboxed as Unboxed

import           GHC.Generics (Generic)

import           P

import           System.IO (IO)
import           System.Random (randomRIO)


data Random =
  Random {
      randomMoves :: [PunterMove]
    , randomWorld :: World
    } deriving (Eq, Show, Generic)

instance Binary Random where

random :: Robot
random =
  Robot "random" init move

init :: PunterId -> PunterCount -> World -> Config -> IO (Initialisation Random)
init _ _ w _ =
  pure $ Initialisation (Random [] w) []

move :: [PunterMove] -> Random -> IO (RobotMove Random)
move g s = do
  let
    previousMoves =
      g <> randomMoves s

    foo =
      catMaybes . with previousMoves $ \x ->
        case x of
          PunterMove _ Pass ->
            Nothing

          PunterMove _ (Claim r) ->
            Just r

          -- FIX add support for splurge
          PunterMove _ (Splurge _) ->
            Nothing

          PunterMove _ (Option r) ->
            Just r

    rivers =
      Unboxed.filter (\r -> not $ r `elem` foo) $ worldRivers (randomWorld s)

    n =
      Unboxed.length rivers

    updated =
      s { randomMoves = previousMoves }

  ix <- randomRIO (0, n - 1)

  case rivers Unboxed.!? ix of
    Nothing ->
      pure $ RobotMove Pass updated
    Just river ->
      pure $ RobotMove (Claim river) updated
