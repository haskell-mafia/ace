{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Ace.Data as Ace
import qualified Ace.Message as Ace
import qualified Ace.Offline as Ace
import qualified Ace.Serial as Ace

import           Data.ByteString (ByteString, hGet, hPut)

import           P

import           System.IO (IO, Handle, print)
import           System.IO (stdin, stdout, hFlush)
import           System.Exit (exitFailure, exitSuccess)

main :: IO ()
main =
  run stdin stdout

run :: Handle -> Handle -> IO ()
run inn out = do

  m <- Ace.readLength inn

  n <- case m of
    Just i ->
      pure i
    Nothing -> do
      print ("bad number" :: ByteString)
      exitFailure

  rest <- hGet inn n

  result <- process rest

  hPut out result
  hFlush out

process :: ByteString -> IO ByteString
process bs =
  case Ace.asWith Ace.toRequest bs of
    Left er -> do
      print $ "bad json: " <> er
      exitFailure
    Right x ->
      case x of
        Ace.OfflineSetup s ->
          let
            r = Ace.setup s
          in
            pure $ Ace.as Ace.fromState r
        Ace.OfflineGameplay g st ->
          let
            Ace.MoveResult _ r = Ace.play g st
          in
            pure $ Ace.as Ace.fromState r
        Ace.OfflineScoring s st -> do
          let
            _ = Ace.score s st
          exitSuccess
