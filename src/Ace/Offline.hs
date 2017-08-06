{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ace.Offline (
    setup
  , play
  , score
  ) where

import qualified Ace.Data.Binary as Binary
import           Ace.Data.Core
import           Ace.Data.Protocol
import           Ace.Data.Robot

import qualified Data.Text as Text

import           P

import           System.IO (IO)
import qualified System.IO as IO


setup :: Robot -> Setup -> IO SetupResult
setup r (Setup p c w config) =
  case r of
    Robot label init _ -> do
      IO.hPutStrLn IO.stderr . Text.unpack $ "Running robot:" <> label
      x <- init p c w config
      pure $ SetupResult p (initialisationFutures x) (State p . Binary.encode . initialisationState $ x)

play :: Robot -> [PunterMove] -> State -> IO (Either Text MoveResult)
play r moves s = do
  case r of
    Robot _ _ move -> do
      case Binary.decode . stateRobot $ s of
        Left msg ->
          pure $ Left msg
        Right v -> do
          m <- move moves v
          pure . Right $ MoveResult (PunterMove (statePunter s) $ robotMoveValue m) (s { stateRobot = Binary.encode . robotMoveState $ m })
