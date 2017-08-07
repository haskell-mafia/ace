{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ace.Robot.Myopia (
    myopia
  ) where

import qualified Ace.Analysis.Score as Score
import           Ace.Data.Config
import           Ace.Data.Core
import           Ace.Data.Robot

import           Data.Binary (Binary)
import qualified Data.Map as Map

import           GHC.Generics (Generic)

import           P

import           System.IO (IO)


data Myopia =
  Myopia {
      myopiaPunter :: PunterId
    , myopiaScoreState :: Score.State
    } deriving (Eq, Show, Generic)

instance Binary Myopia where

-- FIX lens
updateScoreState :: (Score.State -> Score.State) -> Myopia -> Myopia
updateScoreState f g =
  g { myopiaScoreState = f (myopiaScoreState g) }

myopia :: Robot
myopia =
  Robot "myopia" init move

init :: PunterId -> PunterCount -> World -> Config -> IO (Initialisation Myopia)
init punter pcount world _config =
  let
    state =
      Myopia punter (Score.init pcount world)

    futures =
      []
  in
    pure $
      Initialisation state futures

update :: [PunterMove] -> Myopia -> Myopia
update moves state0 =
  updateScoreState (Score.update moves) state0

move :: [PunterMove] -> Myopia -> IO (RobotMove Myopia)
move moves state0 =
  let
    state =
      update moves state0

    scores =
      sortOn (Down . snd) . Map.toList $
        Score.choices (myopiaPunter state) (myopiaScoreState state)
  in
    case scores of
      [] ->
        pure $ RobotMove (Just Pass) state
      (x, _) : _ ->
        pure $ RobotMove (Just $ Claim x) state
