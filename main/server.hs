{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


import           Ace.Data.Robot
import qualified Ace.IO.Offline.Server as Server
import qualified Ace.Robot.Registry as Registry
import qualified Ace.World.Generator as Generator


import qualified Data.List as List
import qualified Data.Text as Text

import qualified Hedgehog.Gen as Gen

import           P

import           System.IO (IO)
import qualified System.IO as IO
import           System.Environment (getArgs)
import           System.Exit (exitFailure)

main :: IO ()
main =
  getArgs >>= \s ->
    case s of
      -- FIX use map argument to pick world or generat
      (_map:executable:_:_:_) -> do
        let
          names = (RobotName . Text.pack) <$> List.drop 2 s
          bots = catMaybes $ fmap Registry.pick names

        unless (length bots == length names) $ do
          IO.hPutStrLn IO.stderr $ "Couldn't find a match for all your requested bots [" <> (Text.unpack . Text.intercalate ", " $ robotName <$> names) <> "]. Available: "
          forM_ Registry.names $ \name -> IO.hPutStrLn IO.stderr $ "  " <> (Text.unpack . robotName) name
          exitFailure

        world <- Gen.sample $ Generator.genWorld_ 20

        Server.run executable names world

      _ -> do
        IO.hPutStr IO.stderr "usage: server MAP EXECUTABLE BOT BOT ..."
        exitFailure
