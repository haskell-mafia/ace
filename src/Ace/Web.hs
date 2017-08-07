{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Ace.Web (
    generateNewId
  , setup
  , move
  , stop
  , calculateStats
  ) where

import           Ace.Data.Core
import           Ace.Data.Offline
import           Ace.Data.Protocol
import           Ace.Data.Web
import           Ace.Data.Robot
import           Ace.Serial

import           Data.Aeson (Value (..), toJSON, encode)
import           Data.Aeson (object, (.=))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Time.Clock.POSIX as Clock

import           P

import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import           System.IO (IO)
import qualified System.Random as Random

import           Text.Printf (printf)

generateNewId :: IO GameId
generateNewId =
  let
    loop n = do
      i <- Random.randomRIO (0, 9999 :: Int)
      p <- Clock.getPOSIXTime
      let
        s = printf "%012d" (floor p :: Integer) <> "_" <> printf "%04d" i
      exists <- Directory.doesDirectoryExist $ gamesPrefix `FilePath.combine` s
      if not $ exists then
        pure . GameId $ Text.pack s
      else if n <= 200 then
        loop (n + 1)
      else
        fail "Could not generate game id"
  in
    loop (0 :: Int)

setup :: World -> GameId -> IO ()
setup world gid = do
  let
    path = gamesPrefix `FilePath.combine` (Text.unpack $ gameId gid)
  Directory.createDirectoryIfMissing True path
  ByteString.writeFile (path `FilePath.combine` "index.html") . Text.encodeUtf8 $
    indexPage gid
  ByteString.writeFile (path `FilePath.combine` "world.json") $
    as fromOnlineState (OnlineState world $ PunterId 0)
  ByteString.writeFile (moves gid) ""

moves :: GameId -> FilePath.FilePath
moves gid =
  let
    path = gamesPrefix `FilePath.combine` (Text.unpack $ gameId gid)
  in
    path `FilePath.combine` "moves.txt"

move :: GameId -> PunterMove -> IO ()
move gid m =
  ByteString.appendFile (moves gid) $
    (Lazy.toStrict . encode $ fromMove m) <> "\n"

stop :: GameId -> World -> [Player] -> [PunterScore] -> IO ()
stop gid w players scores = do
  results gid players scores
  stats gid w players scores

stats :: GameId -> World -> [Player] -> [PunterScore] -> IO ()
stats gid world players scores =
  let
    path = gamesPrefix `FilePath.combine` (Text.unpack $ gameId gid)
    statss = path `FilePath.combine` "stats.json"
  in
    ByteString.writeFile statss $ calculateStats world players scores

results :: GameId -> [Player] -> [PunterScore] -> IO ()
results gid players scores =
  let
    path = gamesPrefix `FilePath.combine` (Text.unpack $ gameId gid)
    result = path `FilePath.combine` "result.json"
  in
    ByteString.writeFile result . as id $
      object [
          "scores" .= fromXs (computeXs players scores)
        ]

computeXs :: [Player] -> [PunterScore] -> [X]
computeXs players scores =
  with scores $ \(PunterScore p s) ->
    case find (\player -> p == playerId player) players of
      Nothing ->
        X p "unknown" s
      Just pl ->
        X p (robotName $ playerRobot pl) s

data X =
  X {
      xPunter :: !PunterId
    , xRobot :: !Text
    , xValue :: !Score
    }

fromXs :: [X] -> Value
fromXs =
  toJSON . fmap fromX

fromX :: X -> Value
fromX x =
  object [
      "punter" .= (fromPunterId . xPunter) x
    , "score" .= (fromScore . xValue) x
    , "name" .= xRobot x
    ]

calculateStats :: World -> [Player] -> [PunterScore] -> ByteString.ByteString
calculateStats _world players scores =
  as id $
    object [
        "scores" .= fromXs (computeXs players scores)
      ]
