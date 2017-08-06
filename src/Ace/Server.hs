{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Ace.Server (
    run
  ) where

import           Ace.Data.Core
import           Ace.Data.Protocol
import qualified Ace.Random.Layout as Layout
import           Ace.Score
import           Ace.Serial

import           Control.Monad.IO.Class (liftIO)

import           Data.ByteString (ByteString)
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector.Unboxed as Unboxed

import qualified Hedgehog.Gen as Gen

import           P

import qualified System.Exit as Exit
import           System.IO (IO)
import qualified System.IO as IO
import qualified System.Process as Process

import           X.Control.Monad.Trans.Either (EitherT, runEitherT, hoistEither, left)

data ServerError =
    ServerParseError Text
  | ServerNoPlayers
    deriving (Eq, Show)

data Player =
  Player {
      playerExecutable :: !IO.FilePath
    , playerId :: !PunterId
    , playerState :: !State
    } deriving (Eq, Show)

run :: [IO.FilePath] -> IO ()
run executables  = do
  world <- Gen.sample $ Layout.genWorld_ 20
  let
    counter = PunterCount (length executables)
  initialised <- forM (List.zip executables [0..]) $ \(executable, n) ->
    orFlail $ setup executable (PunterId n) counter world
  orFlail $ play (Unboxed.length . worldRivers $ world) world initialised []

setup :: IO.FilePath -> PunterId -> PunterCount -> World -> EitherT ServerError IO Player
setup executable pid counter world = do
  r <- liftIO $ execute executable . packet . fromSetup $ Setup pid counter world defaultSettings
  v <- fmap setupResultState . hoistEither . first ServerParseError $
    asWith toSetupResult r
  pure $ Player executable pid v

play :: Int -> World -> [Player] -> [PunterMove] -> EitherT ServerError IO ()
play n world players last =
  if n > 0
    then
      next n world players last
    else
      stop world players last

stop :: World -> [Player] -> [PunterMove] -> EitherT ServerError IO ()
stop world players moves = do
  let
    scores = calculateScore world (PunterCount $ length players) moves
  forM_ players $ \player ->
    liftIO $ execute (playerExecutable player) . packet . fromStop $ Stop moves scores (Just $ playerState player)

next :: Int -> World -> [Player] -> [PunterMove] -> EitherT ServerError IO ()
next n world players last =
  case players of
    (x:xs) -> do
      r <- move x last
      play (n - 1) world (xs <> [Player (playerExecutable x) (playerId x) (moveResultState r)]) (moveResultMove r : last )
    [] ->
      left ServerNoPlayers

move :: Player -> [PunterMove] -> EitherT ServerError IO MoveResult
move player last = do
  r <- liftIO $ execute (playerExecutable player) . packet . fromMoveRequestServer $ MoveRequestServer last (playerState player)
  hoistEither . first ServerParseError $
    asWith toMoveResult r

execute :: IO.FilePath -> ByteString -> IO ByteString
execute executable input =
  fmap (Text.encodeUtf8 . Text.drop 1 . Text.dropWhile (/= ':') . Text.pack) $
    Process.readProcess executable [] (Text.unpack . Text.decodeUtf8 $ input)

orFlail :: EitherT ServerError IO a -> IO a
orFlail x =
  runEitherT x >>= either flail pure

flail :: ServerError -> IO a
flail err = do
  IO.hPutStrLn IO.stderr . Text.unpack . renderServerError $ err
  Exit.exitFailure

renderServerError :: ServerError -> Text
renderServerError err =
  case err of
    ServerParseError msg ->
      "Could not parse response: " <> msg
    ServerNoPlayers ->
      "No players."
