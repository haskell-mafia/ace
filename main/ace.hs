{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}

import qualified Ace.Data as Ace
import qualified Ace.Message as Ace
import qualified Ace.Offline as Ace
import qualified Ace.Robot.Charles as Robot
import qualified Ace.Robot.Ibis as Robot
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
      "--ibis" : [] ->
        runx Robot.ibis
      _ ->
        runx $ Robot.lannister Robot.Cersei


run :: Handle -> Handle -> Ace.Robot a -> IO ()
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

process :: Ace.Robot a -> ByteString -> IO ByteString
process robot bs =
  case Ace.asWith (Ace.toRequest $ Ace.robotDecode robot) bs of
    Left er -> do
      IO.hPutStrLn IO.stderr . Text.unpack $ "bad json: " <> er
      exitFailure
    Right x ->
      case x of
        Ace.OfflineSetup s -> do
          r <- Ace.setup robot s
          pure . Ace.packet $ Ace.fromSetupResult (Ace.fromState $ Ace.robotEncode robot) r
        Ace.OfflineGameplay g st -> do
          r <- Ace.play robot g st
          pure . Ace.packet $ Ace.fromMoveResult (Ace.fromState $ Ace.robotEncode robot) r
        Ace.OfflineScoring s (Ace.State p _ _ _ _) -> do
          if Ace.didIWin p s then do
            IO.hPutStrLn IO.stderr . ppShow . sortOn (Down . Ace.scoreValue) $ Ace.stopScores s
            IO.hPutStrLn IO.stderr . Text.unpack $ "The " <> Ace.robotLabel robot <> " robot won!"
            exitSuccess
          else do
            exitSuccess
