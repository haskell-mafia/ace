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
  , pickMaps
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
    , mapPlayers :: !Int
    } deriving (Eq, Show)

small :: IO [Map]
small = sequence [
    pickX "sample" 2
  , pickX "lambda" 4
  , pickX "circle" 4
  , pickX "Sierpinski-triangle" 3
  ]

medium :: IO [Map]
medium = sequence [
    pickX "tube" 8
  , pickX "randomMedium" 4
  , pickX "randomSparse" 4
  , pickX "boston-sparse" 8
  ]

large :: IO [Map]
large = sequence [
    pickX "edinburgh-sparse" 16
  , pickX "gothenburg-sparse" 16
  , pickX "nara-sparse" 16
  ]

pickX :: Text -> Int -> IO Map
pickX map ze = do
  pick map >>= \w ->
    pure $ Map w map ze

pickMaps :: Text -> IO [Map]
pickMaps map = do
  things <- fmap join . sequence $ [small, medium, large]
  case map of
    "small" ->
      small
    "medium" ->
      medium
    "large" ->
      large
    "all" ->
      pure things
    _ ->
      case List.find ((==) map . mapName) things of
        Nothing -> do
          small
        Just m ->
          pure [m]

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
          pure $ world

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
