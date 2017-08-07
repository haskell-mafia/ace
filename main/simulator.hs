{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}
import           Ace.Data.Core
import           Ace.Data.Config
import           Ace.Data.Offline
import           Ace.Data.Protocol
import           Ace.Data.Robot
import           Ace.Data.Web
import           Ace.Serial
import qualified Ace.IO.Offline.Server as Server
import qualified Ace.Robot.Registry as Robot
import qualified Ace.Web as Web
import qualified Ace.World.Registry as World

import           Control.Concurrent.Async (mapConcurrently)

import           Data.Aeson (object, (.=), toJSON)
import qualified Data.List as List
import           Data.Maybe (fromJust)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString as ByteString

import           P

import           System.IO (IO)
import qualified System.IO as IO
import           System.Environment (getArgs, lookupEnv)
import           System.Exit (exitFailure)

import           X.Control.Monad.Trans.Either.Exit (orDie)

main :: IO ()
main = do
  gameCount <- lookupEnv "SIMULATION_GAMES"
  getArgs >>= \s ->
    case s of
      (map:executable:_:_:_) -> do
        let
          bigint = maybe 20 id $ gameCount >>= readMaybe
          runs = [0 .. bigint :: Int]
--          names = (RobotName . Text.pack) <$> List.drop 2 s
          ns = List.drop 2 s
          names = (\(a, b) -> RobotIdentifier (RobotName . Text.pack $ a) (Punter . Text.pack $ a <> b)) <$> List.zip ns (fmap show [0 :: Int ..])
        validateBots $ fmap identifierName names

        world <- World.pick $ Text.pack map

        g <- Web.generateNewId
        IO.hPutStrLn IO.stderr . Text.unpack $ "Game prefix: " <> (gameId g)
        results <- flip mapConcurrently runs $ \run -> do
          let
            gid = GameId $ gameId g <> Text.pack (show run)
            config = List.head configs

          orDie Server.renderServerError $
            Server.run gid executable names world config

        IO.hPutStr IO.stderr . Text.unpack . Text.decodeUtf8 $
          render world (collectResults (Text.pack map) names results)

      _ -> do
        IO.hPutStr IO.stderr "usage: server MAP EXECUTABLE BOT BOT ..."
        exitFailure

collectResults :: Text -> [RobotIdentifier] -> [[PunterResult]] -> [Result]
collectResults map names games = do
  robot <- names
  let
    fredo = mconcat $ do
      game <- games
      let
        maxScore = List.head $ sortOn (Down . punterResultValue) game
        ownScore = fromJust $ find (\r -> identifierPunter robot == (identifierPunter . punterResultRobot) r) game
      if maxScore /= ownScore then
        [ResultDetail 1 0]
      else
        [ResultDetail 1 1]
  pure $ Result (identifierName robot) (identifierPunter robot) map fredo

render :: World -> [Result] -> ByteString.ByteString
render _world results =
  as toJSON . with results $ \result ->
    object [
        "robot" .= (robotName . resultRobot) result
      , "punter" .= (renderPunter . resultPunter) result
      , "map" .= resultMap result
      , "games" .= (resultDetailGames . resultDetail) result
      , "wins" .= (resultDetailWins . resultDetail) result
      , "winss" .= ((fromIntegral (resultDetailWins . resultDetail $ result) / fromIntegral (resultDetailGames . resultDetail $ result)) * 100 :: Double)
      ]

configs :: [Config]
configs = do
  futures <- [minBound .. maxBound]
  splurges <- [minBound .. maxBound]
  options <- [minBound .. maxBound]
  pure $ Config futures splurges options

validateBots :: [RobotName] -> IO ()
validateBots names = do
  let
    bots = catMaybes $ fmap Robot.pick names
  unless (length bots == length names) $ do
    IO.hPutStrLn IO.stderr $ "Couldn't find a match for all your requested bots [" <> (Text.unpack . Text.intercalate ", " $ robotName <$> names) <> "]. Available: "
    forM_ Robot.names $ \name ->
      IO.hPutStrLn IO.stderr $ "  " <> (Text.unpack . robotName) name
    exitFailure
