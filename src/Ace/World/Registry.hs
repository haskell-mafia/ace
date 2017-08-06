{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Ace.World.Registry (
    worlds
  ) where

import           Ace.Data.Core

import qualified Ace.Serial as Serial

import           Data.ByteString (ByteString)
import           Data.FileEmbed (embedDir)
import qualified Data.Text as Text

import           P

import           System.IO (FilePath)
import           System.FilePath (takeFileName, dropExtension)

worlds :: [(Text, World)]
worlds =
  catMaybes . with onDisk $ \(file, bytes) ->
    let
      name = dropExtension . takeFileName $ file
      world = Serial.asWith Serial.toWorld bytes
    in
      (Text.pack name,) <$> rightToMaybe world

onDisk :: [(FilePath, ByteString)]
onDisk =
  $(embedDir "maps")
