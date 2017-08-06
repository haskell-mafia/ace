{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Ace.IO.Online (
    OnlineError (..)
  , renderOnlineError

  , run
  ) where

import qualified Ace.Data.Binary as Binary
import           Ace.Data.Core
import           Ace.Data.Online
import           Ace.Data.Protocol
import           Ace.Data.Robot
import           Ace.Data.Web
import           Ace.Protocol.Error
import qualified Ace.Protocol.Read as Read
import qualified Ace.Protocol.Write as Write
import           Ace.Serial
import           Control.Monad.IO.Class (liftIO)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as Text

import qualified Network.Simple.TCP as TCP

import           P

import qualified System.Clock as Clock
import           System.IO (IO)
import qualified System.IO as IO

import           Text.Show.Pretty (ppShow)


import           X.Control.Monad.Trans.Either (EitherT, newEitherT, runEitherT, hoistEither, left)

data OnlineError =
    NoHandshakeResponse
  | CouldNotParseHandshake Text
  | HandshakeMismatch Punter Punter
  | CouldNotParseSetup Text
  | NoGameplayResponse
  | CouldNotParseMoves Text
  | CouldNotParseState Text
  | OnlineProtocolError ProtocolError
    deriving (Eq, Show)

run :: Hostname -> Port -> Punter -> Robot -> EitherT OnlineError IO ()
run hostname port punter robot =
  newEitherT . TCP.connect (Text.unpack . getHostname $ hostname) (show . getPort $ port) $ \(socket, _address) -> runEitherT $ do
    let
      reader = Read.fromSocket socket
      writer = Write.fromSocket socket

    handshake reader writer punter
    (Setup p c w config) <- setup reader
    case robot of
      Robot _ init _ -> do
        x <- liftIO $ init p c w config
        Write.message writer . fromSetupResult $ SetupResult p (initialisationFutures x) (State p . Binary.encode $ ())
        liftIO $ ByteString.writeFile "webcloud/games/current/world.json" $ as fromOnlineState (OnlineState w p)
        liftIO $ BSL.writeFile "webcloud/games/current/moves.txt" ""
        stop <- play reader writer robot (State p . Binary.encode $ initialisationState x)
        liftIO $ IO.hPutStrLn IO.stderr . ppShow . sortOn (Down . scoreValue) $ stopScores stop
        liftIO $ if didIWin p stop then
          IO.hPutStrLn IO.stderr . Text.unpack $ "The " <> robotLabel robot <> " robot won!"
        else
          IO.hPutStrLn IO.stderr . Text.unpack $ "The " <> robotLabel robot <> " robot lost!"
        pure ()

handshake :: Read.Reader -> Write.Writer -> Punter -> EitherT OnlineError IO ()
handshake reader writer punter = do
  Write.message writer . fromMe fromPunter $ punter
  msg <- firstT OnlineProtocolError $
    Read.message reader
  res <- hoistEither . first CouldNotParseHandshake $
    asWith (toYou toPunter) msg
  unless (punter == res) $
    left $ HandshakeMismatch punter res

setup :: Read.Reader -> EitherT OnlineError IO Setup
setup reader = do
  msg <- firstT OnlineProtocolError $
    Read.message reader
  hoistEither . first CouldNotParseHandshake $
    asWith toSetup msg

play :: Read.Reader -> Write.Writer -> Robot -> State -> EitherT OnlineError IO Stop
play reader writer robot state =
  case robot of
    Robot _ _ move -> do
      msg <- firstT OnlineProtocolError $
        Read.message reader
      start <- liftIO $ Clock.getTime Clock.Monotonic
      res <- hoistEither . first CouldNotParseMoves $
        asWith toMovesOrStop msg
      case res of
        JustStop stop ->
          pure stop
        JustMoves moves -> do
          liftIO . BSL.appendFile "webcloud/games/current/moves.txt" $
            (Aeson.encode . fmap fromMove $ moves) <> "\n"
          v <- hoistEither . first CouldNotParseState $
            Binary.decode . stateRobot $ state
          m <- liftIO $ move moves v
          end <- liftIO $ Clock.getTime Clock.Monotonic
          liftIO . IO.print $ m
          liftIO . IO.putStrLn $ " ` in: " <> show (Clock.diffTimeSpec end start)
          Write.message writer $ fromMove (PunterMove (statePunter state) $ robotMoveValue m)
          play reader writer robot (state { stateRobot = Binary.encode . robotMoveState $ m })

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
    OnlineProtocolError e ->
      mconcat ["Online protocol error: ", renderProtocolError e]
