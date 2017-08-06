{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Ace.Data.Config
import           Ace.Data.Robot
import           Ace.Data.Web
import qualified Ace.IO.Offline.Server as Server
import qualified Ace.Robot.Registry as Registry
import qualified Ace.World.Generator as Generator
import qualified Ace.World.Registry as Registry

import qualified Data.List as List
import qualified Data.Text as Text

import qualified Hedgehog.Gen as Gen

import           P

import           System.IO (IO)
import qualified System.IO as IO
import           System.Environment (getArgs, lookupEnv)
import           System.Exit (exitFailure)

import           X.Control.Monad.Trans.Either.Exit (orDie)

main :: IO ()
main =
  getArgs >>= \s ->
    case s of
      (map:executable:_:_:_) -> do
        config <-
          Config
            <$> setting "ENABLE_FUTURES" FutureDisabled FutureDisabled FutureEnabled
            <*> setting "ENABLE_SPLURGES" SplurgeDisabled SplurgeDisabled SplurgeEnabled

        let
          names = (RobotName . Text.pack) <$> List.drop 2 s
          bots = catMaybes $ fmap Registry.pick names

        unless (length bots == length names) $ do
          IO.hPutStrLn IO.stderr $ "Couldn't find a match for all your requested bots [" <> (Text.unpack . Text.intercalate ", " $ robotName <$> names) <> "]. Available: "
          forM_ Registry.names $ \name -> IO.hPutStrLn IO.stderr $ "  " <> (Text.unpack . robotName) name
          exitFailure

        world <- case map of
          "random" ->
            Gen.sample $ Generator.genWorld_ 20
          _ ->
            case List.find ((==) map . Text.unpack . fst) Registry.worlds of
              Nothing -> do
                IO.hPutStrLn IO.stderr $ "Couldn't find a match for all your requested world [" <> map <> "]. Available: "
                forM_ Registry.worlds $ \(name, _) -> IO.hPutStrLn IO.stderr $ "  " <> Text.unpack name
                exitFailure
              Just (_, world) ->
                pure world

        gid <- orDie Server.renderServerError $
          Server.run executable names world config
        IO.hPutStrLn IO.stderr . Text.unpack $ "Game: " <> (gameId gid)

      _ -> do
        IO.hPutStr IO.stderr "usage: server MAP EXECUTABLE BOT BOT ..."
        exitFailure


setting :: [Char] -> a -> a -> a -> IO a
setting name dfault disabled enabled =
  with (lookupEnv name) $ \n -> case n of
    Nothing ->
      dfault
    Just "1" ->
      enabled
    Just "t" ->
      enabled
    Just "true" ->
      enabled
    Just "on" ->
      enabled
    _ ->
      disabled
