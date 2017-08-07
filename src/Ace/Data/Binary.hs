{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ace.Data.Binary (
    encode
  , decode
  , decodeBytes
  ) where

import qualified Data.Aeson as Aeson
import           Data.Binary (Binary (..))
import qualified Data.Binary as Binary
import qualified Data.ByteString.Base64.Lazy as Base64
import qualified Data.ByteString.Lazy as Lazy
import           Data.Graph.Inductive.PatriciaTree (Gr)
import           Data.String (String)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           P


encode :: Binary a => a -> Aeson.Value
encode =
  Aeson.toJSON . Text.decodeUtf8 . Lazy.toStrict . Base64.encode . Binary.encode

decode :: Binary a => Aeson.Value -> Either Text a
decode v =
  case Aeson.fromJSON v of
    Aeson.Error msg ->
      Left . Text.pack $ msg
    Aeson.Success t ->
      (first Text.pack . Base64.decode . Lazy.fromStrict . Text.encodeUtf8) t >>=
        decodeBytes

decodeBytes :: Binary a => Lazy.ByteString -> Either Text a
decodeBytes =
  twiddle . Binary.decodeOrFail

twiddle :: Either (Lazy.ByteString, Int64, String) (Lazy.ByteString, Int64, a) -> Either Text a
twiddle e =
  case e of
    Left (_, _, msg) ->
      Left . Text.pack $ msg
    Right ("", _, a) ->
      Right a
    Right (_, _, _) ->
      Left "Decoding binary left bits over..."


instance (Binary a, Binary b) => Binary (Gr a b)
