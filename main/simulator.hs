{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}
import           Ace.Data.Config
import           Ace.Data.Offline
import           Ace.Data.Protocol
import           Ace.Data.Robot
import           Ace.Data.Web
import           Ace.Data.Simulation (combinations)
import qualified Ace.IO.Offline.Server as Server
import qualified Ace.Robot.Registry as Robot
import qualified Ace.Web as Web
import           Ace.World.Registry (Map(..))
import qualified Ace.World.Registry as World

import           Control.Concurrent.Async (mapConcurrently)

import qualified Data.List as List
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
      (mapx:executable:_) -> do
        let
          bigint = maybe 10 id $ gameCount >>= readMaybe
          _runs = [1 .. bigint :: Int]
          _ns = List.drop 2 s
          bots = robotName <$> Robot.names
          names = (\(a, b) -> RobotIdentifier (RobotName a) (Punter $ a <> b)) <$> List.zip bots ((Text.pack . show) <$> [0 :: Int ..])
        validateBots $ fmap identifierName names

        maps <- World.pickMaps (Text.pack mapx)

        g <- Web.generateNewId

        let
          zmaps = List.zip maps [0 :: Int ..]

        results <- flip mapConcurrently zmaps $ \(map, i) -> do
--          x <- flip mapConcurrently runs $ \run -> do
          let
            morenames = combinations (mapPlayers map) names

          x <- forM (List.zip morenames [0 :: Int ..]) $ \(namex, run) -> do
            let
              gid = GameId $ gameId g <> Text.pack (show run) <> Text.pack (show i)
              config = List.head configs

            orDie Server.renderServerError $
              Server.run gid executable namex (mapWorld map) (ServerConfig config False)

          pure (map, x)

--        IO.hPutStr IO.stdout . Text.unpack . Text.decodeUtf8 .
--          render $ collectResults names results
        IO.hPutStr IO.stdout . Text.unpack . Text.decodeUtf8 .
          renderX $ collectResultsX names results

      _ -> do
        IO.hPutStr IO.stderr "usage: server MAP EXECUTABLE BOT BOT ..."
        exitFailure

collectResultsX :: [RobotIdentifier] -> [(Map, [[PunterResult]])] -> [(Map, [(RobotName, ResultDetail)])]
collectResultsX names maps = do
  (map, games) <- maps
  let
    fredo =
      with names $ \robot ->
        let
          v = mconcat $ do
            game <- games
            let
              maxScore = List.head $ sortOn (Down . punterResultValue) game
            case find (\r -> identifierPunter robot == (identifierPunter . punterResultRobot) r) game of
              Nothing ->
                []
              Just ownScore ->
                if maxScore /= ownScore then
                  [ResultDetail 1 0]
                else
                  [ResultDetail 1 1]
        in
          (identifierName robot, v)
  pure $ (map, fredo)

renderX :: [(Map, [(RobotName, ResultDetail)])] -> ByteString.ByteString
renderX xs =
  let
    percentage x = (fromIntegral (resultDetailWins x) / fromIntegral (resultDetailGames x)) * 100 :: Double
  in
    Text.encodeUtf8 . Text.unlines . with xs $ \(map, results) -> mconcat [
        mapName map
      , ": ["
      , Text.intercalate ", " . with (sortOn (Down . percentage . snd) results) $ \(robot, detail) ->
          mconcat [
              robotName robot
            , "("
            , renderIntegral $ resultDetailWins detail
            , "/"
            , renderIntegral $ resultDetailGames detail
            , "/"
            , Text.pack . show $ percentage detail
            , "%"
            , ")"
            ]
      , "]"
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
