{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ace.Robot.Venetian (
    venetian
  ) where

import           Ace.Data
import           Ace.Score

import           Data.Aeson (ToJSON(..), FromJSON(..))
import qualified Data.List as List
import qualified Data.Vector.Unboxed as Unboxed

import           GHC.Generics (Generic)

import           P

import           System.IO (IO)

import           X.Text.Show (gshowsPrec)


data Venetian =
  Venetian {
      venetianBestRoute :: !Route
    , venetianAvailableMineRivers :: !(Unboxed.Vector River)
    } deriving (Eq, Ord, Generic)

instance FromJSON Venetian where
instance FromJSON Route where
instance FromJSON Score where
instance FromJSON River where
instance FromJSON SiteId where
instance ToJSON Venetian where
instance ToJSON Route where
instance ToJSON Score where
instance ToJSON River where
instance ToJSON SiteId where

instance Show Venetian where
  showsPrec =
    gshowsPrec

venetian :: Robot Venetian
venetian =
  Robot "venetian" init move toJSON parseJSON

init :: Setup -> IO (Initialisation Venetian)
init setup = do
  let
    world =
      setupWorld setup

    rivers =
      worldRivers world

    mines =
      worldMines world

    best =
      bestRoute [] Nothing world

    mineRivers =
      Unboxed.filter ((`Unboxed.elem` mines) . riverSource) $ rivers

  pure $ Initialisation (Venetian best mineRivers) []

move :: Gameplay -> State Venetian -> IO (RobotMove Venetian)
move context state = do
  let
    me =
      statePunter state

    world =
      stateWorld state

    brain =
      stateData state

    moves =
      gameplay context

    riverMoves =
      Unboxed.fromList .
      List.concat .
      for moves $ \case
        Claim _ river ->
          [river]
        Pass _ ->
          []

    mineRivers =
      Unboxed.filter (`Unboxed.elem` riverMoves) .
      venetianAvailableMineRivers $
        brain

    rivers =
      worldRivers world

    best =
      venetianBestRoute brain

    expand =
      let
        new =
          bestRoute [] (Just me) world
      in
        if new == best then
          return $ RobotPass brain
        else
          move context $ state { stateData = brain { venetianBestRoute = new } }

  case mineRivers Unboxed.!? 0 of
    -- First try to claim the mine rivers
    Just river ->
      return . RobotClaim brain $ river

    -- Then try to complete the best route
    Nothing ->
      case routePath best Unboxed.!? 0 of
        Just site ->
          case Unboxed.find ((== site) . riverSource) rivers of
            Just segment -> do
              let
                step =
                  Unboxed.drop 1 . routePath $ best
              return . flip RobotClaim segment $
                brain { venetianBestRoute = best { routePath = step }}
            -- If you fail find a new best route
            Nothing ->
              expand
        -- You've done it! Start a new best route
        Nothing ->
          expand
