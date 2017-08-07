{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}

import           Ace.Data.Protocol
import qualified Ace.Data.Robot as Ace
import qualified Ace.IO.Offline.Client as Ace
import           Ace.Protocol.Error
import qualified Ace.Robot.Registry as Registry

import qualified Data.Text as Text

import           P

import qualified System.Clock as Clock
import           System.IO (IO)
import           System.IO (stdin, stdout)
import qualified System.IO as IO
import           System.Environment (getArgs)
import           System.Exit (exitFailure)

import           X.Control.Monad.Trans.Either.Exit (orDie)

main :: IO ()
main = do
  start <- Clock.getTime Clock.Monotonic
  getArgs >>= \s -> do
    (bot, punter) <- case s of
      [] ->
        pure (Registry.primary, Punter "zeta")
      [x] ->
        case Registry.pick (Ace.RobotName . Text.pack $ x) of
          Nothing -> do
            IO.hPutStrLn IO.stderr $ "Couldn't find a match for all your request bot [" <> x <> "]. Available: "
            forM_ Registry.names $ \name -> IO.hPutStrLn IO.stderr $ "  " <> (Text.unpack . Ace.robotName) name
            exitFailure
          Just bot ->
            pure (bot, (Punter . Ace.robotName . Ace.nameOf $ bot))
      [x, y] ->
        case Registry.pick (Ace.RobotName . Text.pack $ x) of
          Nothing -> do
            IO.hPutStrLn IO.stderr $ "Couldn't find a match for all your request bot [" <> x <> "]. Available: "
            forM_ Registry.names $ \name -> IO.hPutStrLn IO.stderr $ "  " <> (Text.unpack . Ace.robotName) name
            exitFailure
          Just bot ->
            pure (bot, Punter $ Text.pack y)
      _ -> do
        IO.hPutStr IO.stderr "usage: punter [BOT]"
        exitFailure

    orDie renderProtocolError $
      Ace.run stdin stdout bot punter

    end <- Clock.getTime Clock.Monotonic
    IO.hPutStrLn IO.stderr . Text.unpack . Ace.robotName $ Ace.nameOf bot
    IO.hPutStrLn IO.stderr $ " ` in: " <> show (Clock.diffTimeSpec end start)
