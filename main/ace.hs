{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}

import qualified Ace.Data.Protocol as Ace
import qualified Ace.Data.Robot as Ace
import qualified Ace.Message as Ace
import qualified Ace.IO.Offline.Client as Ace
import qualified Ace.Robot.Charles as Robot
import qualified Ace.Robot.Lannister as Robot
import qualified Ace.Robot.Random as Robot
import qualified Ace.Robot.Silver as Robot
import qualified Ace.Serial as Ace

import           Data.ByteString (ByteString, hGet, hPut)
import qualified Data.Text as Text
import           Text.Show.Pretty (ppShow)

import           P

import           System.IO (IO, Handle, print)
import           System.IO (stdin, stdout, hFlush)
import qualified System.IO as IO
import           System.Environment (getArgs)
import           System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  let
    runx robot = run stdin stdout robot
  getArgs >>= \s ->
    case s of
      "--charles" : [] ->
        runx Robot.charles
      "--cersei" : [] ->
        runx $ Robot.lannister Robot.Cersei
      "--tyrion" : [] ->
        runx $ Robot.lannister Robot.Tyrion
      "--lannister" : [] ->
        runx $ Robot.lannister Robot.Tyrion
      "--silver" : [] ->
        runx $ Robot.silver
      "--random" : [] ->
        runx Robot.random
      _ ->
        runx Robot.silver


run :: Handle -> Handle -> Ace.Robot -> IO ()
run inn out robot = do

  m <- Ace.readLength inn

  n <- case m of
    Just i ->
      pure i
    Nothing -> do
      print ("bad number" :: ByteString)
      exitFailure

  rest <- hGet inn n

  result <- process robot rest

  hPut out result
  hFlush out

process :: Ace.Robot -> ByteString -> IO ByteString
process robot bs =
  case Ace.asWith Ace.toRequest bs of
    Left er -> do
      IO.hPutStrLn IO.stderr . Text.unpack $ "bad json: " <> er
      exitFailure
    Right x ->
      case x of
        Ace.OfflineSetup s -> do
          r <- Ace.setup robot s
          pure . Ace.packet $ Ace.fromSetupResult r
        Ace.OfflineGameplay g st -> do
          r <- Ace.play robot (Ace.gameplay g) st
          case r of
            Left err -> do
              IO.hPutStr IO.stderr $ "Error making move: " <> Text.unpack err
              exitFailure
            Right result ->
              pure . Ace.packet $ Ace.fromMoveResult result
        Ace.OfflineScoring s (Ace.State p _) -> do
          if Ace.didIWin p s then do
            IO.hPutStrLn IO.stderr . ppShow . sortOn (Down . Ace.scoreValue) $ Ace.stopScores s
            IO.hPutStrLn IO.stderr . Text.unpack $ "The " <> Ace.robotLabel robot <> " robot won!"
            exitSuccess
          else do
            exitSuccess
