{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Ace.Online (
    run
  ) where

import           Ace.Data
import           Ace.Message
import           Ace.Serial

import           Control.Monad.IO.Class (liftIO)

import qualified Data.Text as Text

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
    deriving (Eq, Show)

run :: Hostname -> Port -> Punter -> IO ()
run hostname port punter =
  TCP.connect (Text.unpack . getHostname $ hostname) (show . getPort $ port) $ \(socket, _address) -> do
    orFlail $ handshake socket punter

handshake :: TCP.Socket -> Punter -> EitherT OnlineError IO ()
handshake socket punter = do
  liftIO $ TCP.send socket . packet . fromMe fromPunter $ punter
  msg <- eitherTFromMaybe NoHandshakeResponse $
    readMessage' (\n -> TCP.recv socket n >>= maybe (pure "") pure)
  res <- hoistEither . first CouldNotParseHandshake $
    asWith (toYou toPunter) msg
  unless (punter == res) $
    left $ HandshakeMismatch punter res

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
    HandshakeMismatch expected got ->
      mconcat ["Handshake response did not match, expected: ", renderPunter expected, ", got: ", renderPunter got]
