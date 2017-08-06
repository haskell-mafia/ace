{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ace.IO.Offline.Server (
    ServerError (..)
  , renderServerError
  , run
  ) where

import qualified Ace.Data.Binary as Binary
import           Ace.Data.Config
import           Ace.Data.Core
import           Ace.Data.Offline
import           Ace.Data.Protocol
import           Ace.Data.Web
import           Ace.Data.Robot
import           Ace.Score
import           Ace.Serial
import qualified Ace.Web as Web

import           Control.Monad.IO.Class (liftIO)

import           Data.ByteString (ByteString)
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector.Unboxed as Unboxed


import           P

import           System.IO (IO)
import qualified System.IO as IO
import qualified System.Process as Process

import           X.Control.Monad.Trans.Either (EitherT, hoistEither, left)


data ServerError =
    ServerParseError Text
  | ServerNoPlayers
    deriving (Eq, Show)

run :: IO.FilePath -> [RobotName] -> World -> EitherT ServerError IO GameId
run executable robots world = do
  gid <- liftIO $ Web.generateNewId
  liftIO $ Web.setup world gid
  let
    counter = PunterCount (length robots)
  initialised <- forM (List.zip robots [0..]) $ \(robot, n) ->
    setup executable robot (PunterId n) counter world
  play (Unboxed.length . worldRivers $ world) gid world initialised []
  pure gid

setup :: IO.FilePath -> RobotName -> PunterId -> PunterCount -> World -> EitherT ServerError IO Player
setup executable robot pid counter world = do
  let
    player = Player executable robot pid (State pid . Binary.encode $ ())
  r <- liftIO $ execute player  . packet . fromSetup $
    Setup pid counter world defaultConfig
  v <- fmap setupResultState . hoistEither . first ServerParseError $
    asWith toSetupResult r
  pure $ player { playerState = v }

play :: Int -> GameId -> World -> [Player] -> [PunterMove] -> EitherT ServerError IO ()
play n gid world players last =
  if n > 0
    then
      next n gid world players last
    else
      stop gid world players last

stop :: GameId -> World -> [Player] -> [PunterMove] -> EitherT ServerError IO ()
stop gid world players moves = do
  let
    scores = calculateScore world (PunterCount $ length players) moves
  liftIO $ Web.stop gid world players scores
  forM_ players $ \player ->
    liftIO $ execute player . packet . fromStop $ Stop moves scores (Just $ playerState player)

next :: Int -> GameId -> World -> [Player] -> [PunterMove] -> EitherT ServerError IO ()
next n gid world players last =
  case players of
    (x:xs) -> do
      r <- move x (List.take (List.length players) last)
      liftIO $ Web.move gid (moveResultMove r)
      play (n - 1) gid world (xs <> [x { playerState = moveResultState r }]) (moveResultMove r : last)
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

renderServerError :: ServerError -> Text
renderServerError err =
  case err of
    ServerParseError msg ->
      "Could not parse response: " <> msg
    ServerNoPlayers ->
      "No players."
