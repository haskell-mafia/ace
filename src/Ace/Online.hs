{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Ace.Online (
    run
  ) where

import qualified Ace.Data.Binary as Binary
import           Ace.Data.Core
import           Ace.Data.Online
import           Ace.Data.Protocol
import           Ace.Data.Robot
import           Ace.Message
import           Ace.Serial

import           Control.Monad.IO.Class (liftIO)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as Text

import qualified Network.Simple.TCP as TCP

import           P

import qualified System.Clock as Clock
import qualified System.Exit as Exit
import           System.IO (IO)
import qualified System.IO as IO

import           Text.Show.Pretty (ppShow)


import           X.Control.Monad.Trans.Either (EitherT, runEitherT, hoistEither, eitherTFromMaybe, left)

data OnlineError =
    NoHandshakeResponse
  | CouldNotParseHandshake Text
  | HandshakeMismatch Punter Punter
  | CouldNotParseSetup Text
  | NoGameplayResponse
  | CouldNotParseMoves Text
  | CouldNotParseState Text
    deriving (Eq, Show)

run :: Hostname -> Port -> Punter -> Robot -> IO ()
run hostname port punter robot =
  TCP.connect (Text.unpack . getHostname $ hostname) (show . getPort $ port) $ \(socket, _address) -> do
    orFlail $ handshake socket punter
    (Setup p c w settings) <- orFlail $ setup socket
    case robot of
      Robot _ init _ -> do
        x <- init p c w (futuresSettings settings)
        liftIO $ TCP.send socket . packet . fromSetupResult $ SetupResult p (initialisationFutures x) (State p . Binary.encode $ ())
        ByteString.writeFile "webclound/world.js" $ "var world = " <> as fromWorld w <> ";"
        IO.appendFile "webcloud/world.js" $ "\nvar player = " <> (show . punterId $ p) <> ";"
        BSL.writeFile "webcloud/moves.txt" ""
        stop <- orFlail $ play socket robot (State p . Binary.encode $ initialisationState x)
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
  hoistEither . first CouldNotParseHandshake $
    asWith toSetup msg

play :: TCP.Socket -> Robot -> State -> EitherT OnlineError IO Stop
play socket robot state =
  case robot of
    Robot _ _ move -> do
      msg <- eitherTFromMaybe NoGameplayResponse $
        readMessage' (\n -> TCP.recv socket n >>= maybe (pure "") pure)
      start <- liftIO $ Clock.getTime Clock.Monotonic
      res <- hoistEither . first CouldNotParseMoves $
        asWith toMovesOrStop msg
      case res of
        JustStop stop ->
          pure stop
        JustMoves moves -> do
          liftIO . BSL.appendFile "webcloud/moves.txt" $
            (Aeson.encode . fmap fromMove $ moves) <> "\n"
          v <- hoistEither . first CouldNotParseState $
            Binary.decode . stateRobot $ state
          m <- liftIO $ move moves v
          end <- liftIO $ Clock.getTime Clock.Monotonic
          liftIO . IO.print $ m
          liftIO . IO.putStrLn $ " ` in: " <> show (Clock.diffTimeSpec end start)
          liftIO $ TCP.send socket . packet $ fromMove (PunterMove (statePunter state) $ robotMoveValue m)
          play socket robot (state { stateRobot = Binary.encode . robotMoveState $ m })

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
    CouldNotParseState msg ->
      "Could not parse state response: " <> msg
