{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Ace.Web (
    generateNewId
  , setup
  , move
  ) where

import           Ace.Data.Core
import           Ace.Data.Web
import           Ace.Serial

import           Data.Aeson (encode)
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
      else if n <= 20 then
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
