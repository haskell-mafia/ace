{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Ace.Protocol.Read (
    Reader (..)
  , fromHandle
  , fromSocket

  , size
  , message

  , movesOrStop
  , setup
  , you
  , offline
  ) where

import           Ace.Data.Protocol
import           Ace.Protocol.Error
import qualified Ace.Serial as Serial

import           Control.Monad.IO.Class (liftIO)

import           Data.Aeson (Value)
import           Data.Aeson.Types (Parser)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lex.Integral as Lex

import           Network.Simple.TCP (Socket)
import qualified Network.Simple.TCP as TCP

import           P

import           System.IO (IO, Handle)

import           X.Control.Monad.Trans.Either (EitherT, left, hoistEither)
--
-- "Best effort" try to read at most Int bytes.
--
-- Empty string is end of input.
--
newtype Reader =
  Reader {
      runReader :: Int -> IO ByteString
    }

fromHandle :: Handle -> Reader
fromHandle =
  Reader . ByteString.hGetSome

fromSocket :: Socket -> Reader
fromSocket socket =
  Reader $ \n ->
    let
      read =
        TCP.recv socket n >>= \x -> case x of
          Nothing ->
            pure ""
          Just "" ->
            read
          Just bytes ->
            pure bytes
    in
      read

size :: Reader -> EitherT ProtocolError IO Int
size reader =
  let
    go acc = do
      c <- liftIO $ runReader reader 1
      if c == ":" then
        case Lex.readDecimal acc of
          Just (i :: Int, "") ->
            pure $! i
          _ ->
            left ProtocolReadSizeError
      else
        if ByteString.length c > 9 then
          left ProtocolReadSizeError
        else
          go $ acc `ByteString.append` c
  in
    go ""

message :: Reader -> EitherT ProtocolError IO ByteString
message reader = do
  l <- size reader
  let
    go acc required = do
      xs <- liftIO $ runReader reader required
      if ByteString.length xs == required then
         pure (acc <> xs)
      else if ByteString.null xs then
         left ProtocolReadMessageError
      else
         go (acc <> xs) (required - ByteString.length xs)
  go "" l

decode :: Reader -> Text -> (Value -> Parser a) -> EitherT ProtocolError IO a
decode reader tag parser =
  message reader >>= hoistEither . first (ProtocolReadDecodeError tag) .  Serial.asWith parser

movesOrStop :: Reader -> EitherT ProtocolError IO MovesOrStop
movesOrStop reader =
  decode reader "MovesOrStop" Serial.toMovesOrStop

setup :: Reader -> EitherT ProtocolError IO Setup
setup reader =
  decode reader "Setup" Serial.toSetup

you :: Reader -> EitherT ProtocolError IO Punter
you reader =
  decode reader "You[Punter]" (Serial.toYou Serial.toPunter)

offline :: Reader -> EitherT ProtocolError IO OfflineRequest
offline reader =
  decode reader "OfflineRequest" Serial.toRequest
