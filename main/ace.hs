{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}

import qualified Ace.Data.Robot as Ace
import qualified Ace.IO.Offline.Client as Ace
import           Ace.Protocol.Error
import qualified Ace.Robot.Registry as Registry

import qualified Data.Text as Text

import           P

import           System.IO (IO)
import           System.IO (stdin, stdout)
import qualified System.IO as IO
import           System.Environment (getArgs)
import           System.Exit (exitFailure)

import           X.Control.Monad.Trans.Either.Exit (orDie)

main :: IO ()
main =
  getArgs >>= \s -> do
    bot <- case s of
      [] ->
        pure Registry.primary
      [x] ->
        case Registry.pick (Ace.RobotName . Text.pack $ x) of
          Nothing -> do
            IO.hPutStrLn IO.stderr $ "Couldn't find a match for all your request bot [" <> x <> "]. Available: "
            forM_ Registry.names $ \name -> IO.hPutStrLn IO.stderr $ "  " <> (Text.unpack . Ace.robotName) name
            exitFailure
          Just bot ->
            pure bot
      _ -> do
        IO.hPutStr IO.stderr "usage: punter [BOT]"
        exitFailure

    orDie renderProtocolError $
      Ace.run stdin stdout bot
