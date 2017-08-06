{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ace.IO.Offline.Server (
    run
  ) where

import qualified Ace.Data.Binary as Binary
import           Ace.Data.Config
import           Ace.Data.Core
import           Ace.Data.Protocol
import           Ace.Data.Web
import           Ace.Data.Robot
import           Ace.Score
import           Ace.Serial

import           Control.Monad.IO.Class (liftIO)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector.Unboxed as Unboxed


import           P

import qualified System.Directory as Directory
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
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
    , playerRobot :: RobotName
    , playerId :: !PunterId
    , playerState :: !State
    } deriving (Eq, Show)

run :: IO.FilePath -> [RobotName] -> World -> IO ()
run executable robots world = do
  gid <- generateNewId
  let
    path = gamesPrefix `FilePath.combine` (Text.unpack $ gameId gid)
    moves = path `FilePath.combine` "moves.txt"
  Directory.createDirectoryIfMissing True path
  ByteString.writeFile (path `FilePath.combine` "index.html") . Text.encodeUtf8 $
    indexPage gid
  ByteString.writeFile (path `FilePath.combine` "world.json") $
    as fromOnlineState (OnlineState world $ PunterId 0)
  ByteString.writeFile moves ""
  let
    counter = PunterCount (length robots)
  initialised <- forM (List.zip robots [0..]) $ \(robot, n) ->
    orFlail $ setup executable robot (PunterId n) counter world
  orFlail $ play (Unboxed.length . worldRivers $ world) world initialised []

setup :: IO.FilePath -> RobotName -> PunterId -> PunterCount -> World -> EitherT ServerError IO Player
setup executable robot pid counter world = do
  let
    player = Player executable robot pid (State pid . Binary.encode $ ())
  r <- liftIO $ execute player  . packet . fromSetup $
    Setup pid counter world defaultConfig
  v <- fmap setupResultState . hoistEither . first ServerParseError $
    asWith toSetupResult r
  pure $ player { playerState = v }

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
    liftIO $ execute player . packet . fromStop $ Stop moves scores (Just $ playerState player)

next :: Int -> World -> [Player] -> [PunterMove] -> EitherT ServerError IO ()
next n world players last =
  case players of
    (x:xs) -> do
      r <- move x last
      play (n - 1) world (xs <> [Player (playerExecutable x) (playerRobot x) (playerId x) (moveResultState r)]) (moveResultMove r : last )
    [] ->
      left ServerNoPlayers

move :: Player -> [PunterMove] -> EitherT ServerError IO MoveResult
move player last = do
  r <- liftIO $ execute player . packet . fromMoveRequestServer $ MoveRequestServer last (playerState player)
  hoistEither . first ServerParseError $
    asWith toMoveResult r

execute :: Player -> ByteString -> IO ByteString
execute player input =
  fmap (Text.encodeUtf8 . Text.drop 1 . Text.dropWhile (/= ':') . Text.pack) $
    Process.readProcess (playerExecutable player) [Text.unpack . robotName . playerRobot $ player] (Text.unpack . Text.decodeUtf8 $ input)

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
