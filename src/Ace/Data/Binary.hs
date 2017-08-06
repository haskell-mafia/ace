{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ace.Data.Binary (
    encode
  , decode
  ) where


import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Base64.Lazy as Base64
import           Data.Binary (Binary (..))
import qualified Data.Binary as Binary
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
        twiddle . Binary.decodeOrFail

twiddle :: Either (Lazy.ByteString, Int64, [Char]) (Lazy.ByteString, Int64, a) -> Either Text a
twiddle e =
  case e of
    Left (_, _, msg) ->
      Left . Text.pack $ msg
    Right ("", _, a) ->
      Right a
    Right (_, _, _) ->
      Left "Decoding binary left bits over..."
