{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Ace.Data as Ace
import qualified Ace.Message as Ace
import qualified Ace.Offline as Ace
import qualified Ace.Robot.Charles as Robot
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

  result <- process Robot.charles rest

  hPut out result
  hFlush out

process :: Ace.Robot a -> ByteString -> IO ByteString
process robot bs =
  case Ace.asWith (Ace.toRequest $ Ace.robotDecode robot) bs of
    Left er -> do
      print $ "bad json: " <> er
      exitFailure
    Right x ->
      case x of
        Ace.OfflineSetup s -> do
          r <- Ace.setup robot s
          pure . Ace.packet $ Ace.fromSetupResult (Ace.robotEncode robot) r
        Ace.OfflineGameplay g st -> do
          r <- Ace.play robot g st
          pure . Ace.packet $ Ace.fromMoveResult (Ace.robotEncode robot) r
        Ace.OfflineScoring _s _st -> do
          exitSuccess
