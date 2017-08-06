{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Ace.Protocol.Write (
    Writer (..)
  , fromHandle
  , fromSocket

  , message
  , move
  , me
  , setupResult
  , moveResult
  ) where

import           Ace.Data.Core
import           Ace.Data.Protocol

import qualified Ace.Serial as Serial

import           Control.Monad.IO.Class (MonadIO (..))

import           Data.Aeson (Value)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString

import           Network.Simple.TCP (Socket)
import qualified Network.Simple.TCP as TCP

import           P

import           System.IO (IO, Handle)
import qualified System.IO as IO


newtype Writer =
  Writer {
      runWriter :: ByteString -> IO ()
    }

fromHandle :: Handle -> Writer
fromHandle h =
  Writer $ \bytes -> ByteString.hPut h bytes >> IO.hFlush h

fromSocket :: Socket -> Writer
fromSocket socket =
  Writer $ TCP.send socket

message :: MonadIO m => Writer -> Value -> m ()
message writer =
  liftIO . runWriter writer . Serial.packet

move :: MonadIO m => Writer -> PunterMove -> m ()
move writer =
  message writer . Serial.fromMove

me :: MonadIO m => Writer -> Punter -> m ()
me writer =
  message writer . Serial.fromMe Serial.fromPunter

setupResult :: MonadIO m => Writer -> SetupResult -> m ()
setupResult writer =
  message writer . Serial.fromSetupResult

moveResult :: MonadIO m => Writer -> MoveResult -> m ()
moveResult writer =
  message writer . Serial.fromMoveResult
