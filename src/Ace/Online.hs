{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Ace.Online (
    run
  ) where

import           Ace.Data
import           Ace.Message
import           Ace.Serial

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL

import           Control.Monad.IO.Class (liftIO)

import qualified Data.Text as Text
import           Text.Show.Pretty (ppShow)

import qualified Network.Simple.TCP as TCP

import           P

import qualified System.Exit as Exit
import           System.IO (IO)
import qualified System.IO as IO

import           X.Control.Monad.Trans.Either (EitherT, runEitherT, hoistEither, eitherTFromMaybe, left)

data OnlineError =
    NoHandshakeResponse
  | CouldNotParseHandshake Text
  | HandshakeMismatch Punter Punter
  | CouldNotParseSetup Text
  | NoGameplayResponse
  | CouldNotParseMoves Text
    deriving (Eq, Show)

run :: Show a => Hostname -> Port -> Punter -> Robot a -> IO ()
run hostname port punter robot =
  TCP.connect (Text.unpack . getHostname $ hostname) (show . getPort $ port) $ \(socket, _address) -> do
    orFlail $ handshake socket punter
    -- FIX use settings
    s@(Setup p c w _settings) <- orFlail $ setup socket
--    IO.print s
    x <- robotInit robot s
    dumpAsJson w
    IO.writeFile "webcloud/moves.js" $ "var player = " <> (Text.unpack . renderPunterId $ p) <> ";\n"
    BSL.appendFile "webcloud/moves.js" "var moves = [];\n"
    stop <- orFlail $ play socket robot (State p c w x)
--    IO.print stop
    IO.hPutStrLn IO.stderr . ppShow . sortOn (Down . scoreValue) $ stopScores stop
    if didIWin p stop then
      IO.hPutStrLn IO.stderr . Text.unpack $ "The " <> robotLabel robot <> " robot won!"
    else
      IO.hPutStrLn IO.stderr . Text.unpack $ "The " <> robotLabel robot <> " robot lost!"
    pure ()

handshake :: TCP.Socket -> Punter -> EitherT OnlineError IO ()
handshake socket punter = do
  liftIO $ TCP.send socket . packet . fromMe fromPunter $ punter
  msg <- eitherTFromMaybe NoHandshakeResponse $
    readMessage' (\n -> TCP.recv socket n >>= maybe (pure "") pure)
  res <- hoistEither . first CouldNotParseHandshake $
    asWith (toYou toPunter) msg
  unless (punter == res) $
    left $ HandshakeMismatch punter res

setup :: TCP.Socket -> EitherT OnlineError IO Setup
setup socket = do
  msg <- eitherTFromMaybe NoHandshakeResponse $
    readMessage' (\n -> TCP.recv socket n >>= maybe (pure "") pure)
  initial <- hoistEither . first CouldNotParseHandshake $
    asWith toSetup msg
  liftIO $ TCP.send socket . packet . fromSetupResultOnline . setupPunter $ initial
  pure initial

play :: Show a => TCP.Socket -> Robot a -> State a -> EitherT OnlineError IO (Stop a)
play socket robot state = do
  msg <- eitherTFromMaybe NoGameplayResponse $
    readMessage' (\n -> TCP.recv socket n >>= maybe (pure "") pure)
  res <- hoistEither . first CouldNotParseMoves $
    asWith (toMovesOrStop (robotDecode robot)) msg
  case res of
    JustStop stop ->
      pure stop
    JustMoves moves -> do
      liftIO . BSL.appendFile "webcloud/moves.js" $
        "moves = moves.concat(" <> (Aeson.encode . fmap fromMove $ moves) <> ");\n"
      m <- liftIO $ robotMove robot (Gameplay moves) state
      let
        mv = fromRobotMove state m
--      liftIO . IO.print $ show (moves)
      liftIO . IO.print $ moveResultMove mv

      liftIO $ TCP.send socket . packet $ fromMove (moveResultMove mv)
      play socket robot (moveResultState mv)

orFlail :: EitherT OnlineError IO a -> IO a
orFlail x =
  runEitherT x >>= either flail pure

flail :: OnlineError -> IO a
flail err = do
  IO.hPutStr IO.stderr . Text.unpack . renderOnlineError $ err
  Exit.exitFailure

renderOnlineError :: OnlineError -> Text
renderOnlineError err =
  case err of
    NoHandshakeResponse ->
      "Didn't get a response during handshake."
    CouldNotParseHandshake msg ->
      "Could not parse handshake response: " <> msg
    CouldNotParseSetup msg ->
      "Could not parse setup response: " <> msg
    HandshakeMismatch expected got ->
      mconcat ["Handshake response did not match, expected: ", renderPunter expected, ", got: ", renderPunter got]
    NoGameplayResponse ->
      "Didn't get a response during gameplay."
    CouldNotParseMoves msg ->
      "Could not parse moves response: " <> msg
