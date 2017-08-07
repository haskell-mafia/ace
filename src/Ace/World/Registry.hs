{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Ace.World.Registry (
    Map(..)
  , small
  , medium
  , large
  , pick
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


data Map =
  Map {
      mapWorld :: !World
    , mapName :: !Text
    } deriving (Eq, Show)

small :: IO [Map]
small = sequence [
    pick "sample"
  , pick "lambda"
  , pick "circle"
  , pick "Sierpinski-triangle"
  ]

medium :: IO [Map]
medium = sequence [
    pick "tube"
  , pick "randomMedium"
  , pick "randomSparse"
  , pick "boston-sparse"
  ]

large :: IO [Map]
large = sequence [
    pick "edinburgh-sparse"
  , pick "gothenburg-sparse"
  , pick "nara-sparse"
  ]


-- FIX errors
pick :: Text -> IO Map
pick map =
  case map of
    "random" ->
      fmap (\w -> Map w "random") . Gen.sample $ Generator.genWorld_ 20
    _ ->
      case List.find ((==) map . fst) worlds of
        Nothing -> do
          IO.hPutStrLn IO.stderr $ "Couldn't find a match for all your requested world [" <> Text.unpack map <> "]. Available: "
          forM_ worlds $ \(name, _) ->
            IO.hPutStrLn IO.stderr $ "  " <> Text.unpack name
          exitFailure
        Just (n, world) ->
          pure $ Map world n

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
