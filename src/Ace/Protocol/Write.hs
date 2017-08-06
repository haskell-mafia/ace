{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Ace.Protocol.Write (
    Writer (..)
  , fromHandle
  , fromSocket

  , message
  ) where

import qualified Ace.Serial as Serial

import           Control.Monad.IO.Class (MonadIO (..))

import           Data.Aeson (Value)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString

import           Network.Simple.TCP (Socket)
import qualified Network.Simple.TCP as TCP

import           P

import           System.IO (IO, Handle)


newtype Writer =
  Writer {
      runWriter :: ByteString -> IO ()
    }

fromHandle :: Handle -> Writer
fromHandle =
  Writer . ByteString.hPut

fromSocket :: Socket -> Writer
fromSocket socket =
  Writer $ TCP.send socket

message :: MonadIO m => Writer -> Value -> m ()
message writer =
  liftIO . runWriter writer . Serial.packet
