{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Ace.Protocol.Error (
    ProtocolError (..)
  , renderProtocolError
  ) where

import           P


data ProtocolError =
    ProtocolReadSizeError
  | ProtocolReadMessageError
  | ProtocolDecodeStateError Text
  | ProtocolPlaceholderError Text
    deriving (Eq, Show)

renderProtocolError :: ProtocolError -> Text
renderProtocolError err =
  case err of
    ProtocolReadSizeError ->
      "Error reading size from input."
    ProtocolReadMessageError ->
      "Error reading message from input."
    ProtocolDecodeStateError msg ->
      mconcat ["Error decoding state: ", msg]
    ProtocolPlaceholderError msg ->
      mconcat ["Should be a better error here but is just the following string at the moment: ", msg]
