{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Ace.IO.Offline.Client (
    setup
  , play
  , score
  , run
  ) where

import qualified Ace.Data.Binary as Binary
import           Ace.Data.Core
import           Ace.Data.Protocol
import           Ace.Data.Robot
import           Ace.Protocol.Error
import qualified Ace.Protocol.Read as Read
import qualified Ace.Protocol.Write as Write

import           Control.Monad.IO.Class (liftIO)

import qualified Data.Text as Text

import           P

import           System.IO (IO)
import qualified System.IO as IO

import           Text.Show.Pretty (ppShow)

import           X.Control.Monad.Trans.Either (EitherT, left)


setup :: Robot -> Setup -> IO SetupResult
setup r (Setup p c w config) =
  case r of
    Robot _ init _ -> do
      x <- init p c w config
      pure $ SetupResult p (initialisationFutures x) (State p . Binary.encode . initialisationState $ x)

play :: Robot -> [PunterMove] -> State -> EitherT ProtocolError IO MoveResult
play r moves s = do
  case r of
    Robot _ _ move -> do
      case Binary.decode . stateRobot $ s of
        Left msg ->
          left $ ProtocolDecodeStateError msg
        Right v -> do
          m <- liftIO $ move moves v
          pure  $ MoveResult (PunterMove (statePunter s) $ robotMoveValue m) (s { stateRobot = Binary.encode . robotMoveState $ m })

run :: IO.Handle -> IO.Handle -> Robot -> EitherT ProtocolError IO ()
run inn out robot = do
  let
    reader = Read.fromHandle inn
    writer = Write.fromHandle out

  Read.offline reader >>= \x -> case x of
    OfflineSetup s ->
      liftIO $ setup robot s >>=  Write.setupResult writer
    OfflineGameplay g st ->
      play robot (gameplay g) st >>= Write.moveResult writer
    OfflineScoring s (State p _) -> do
      if didIWin p s then do
        liftIO $ IO.hPutStrLn IO.stderr . ppShow . sortOn (Down . scoreValue) $ stopScores s
        liftIO $ IO.hPutStrLn IO.stderr . Text.unpack $ "The " <> robotLabel robot <> " robot won!"
        pure ()
      else
        pure ()
