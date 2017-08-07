{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Ace.World.Registry (
    pick
  , worlds
  ) where

import           Ace.Data.Core
import qualified Ace.Serial as Serial
import qualified Ace.World.Generator as Generator

import           Data.ByteString (ByteString)
import           Data.FileEmbed (embedDir)
import qualified Data.List as List
import qualified Data.Text as Text

import qualified Hedgehog.Gen as Gen

import           P

import           System.IO (IO, FilePath)
import qualified System.IO as IO
import           System.Exit (exitFailure)
import           System.FilePath (takeFileName, dropExtension)

-- FIX errors
pick :: Text -> IO World
pick map =
  case map of
    "random" ->
      Gen.sample $ Generator.genWorld_ 20
    _ ->
      case List.find ((==) map . fst) worlds of
        Nothing -> do
          IO.hPutStrLn IO.stderr $ "Couldn't find a match for all your requested world [" <> Text.unpack map <> "]. Available: "
          forM_ worlds $ \(name, _) ->
            IO.hPutStrLn IO.stderr $ "  " <> Text.unpack name
          exitFailure
        Just (_, world) ->
          pure world

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
