{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ace.Robot.Lannister (
    LannisterName(..)
  , Lannister(..)
  , lannister
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


data LannisterName =
    Cersei
  | Tyrion
    deriving (Eq, Show, Generic)

data Lannister =
  Lannister {
      lannisterMoves :: [PunterMove]
    , lannisterWorld :: World
    } deriving (Eq, Show, Generic)

instance Binary Lannister where

lannister :: LannisterName -> Robot
lannister l =
  Robot (renderLannister l) init (move l)

renderLannister :: LannisterName -> Text
renderLannister l =
  case l of
    Cersei ->
      "witch"
    Tyrion ->
      "lion"

init :: PunterId -> PunterCount -> World -> Config -> IO (Initialisation Lannister)
init _ _ w _ =
  pure $ Initialisation (Lannister [] w) []

move :: LannisterName -> [PunterMove] -> Lannister -> IO (RobotMove Lannister)
move l g s = do
  let
    previousMoves =
      g <> lannisterMoves s

    foo =
      fmap punterMoveValue previousMoves >>=
        moveRivers

    rivers =
      Unboxed.filter (\r -> not $ r `elem` foo) $ worldRivers (lannisterWorld s)

    mines =
      Unboxed.map getMineId $ worldMines (lannisterWorld s)

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

  let
    updated = (s { lannisterMoves = previousMoves })

  case prefered of
    Just river ->
      pure $ RobotMove (Just $ Claim river) updated
    Nothing ->
      case rivers Unboxed.!? ix of
        Nothing ->
          pure $ RobotMove (Just Pass) updated
        Just river ->
          pure $ RobotMove (Just $ Claim river) updated
