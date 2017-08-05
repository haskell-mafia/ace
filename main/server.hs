{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Ace.Server as Server

import           P

import           System.IO (IO)
import qualified System.IO as IO
import           System.Environment (getArgs, lookupEnv)
import           System.Exit (exitFailure)

main :: IO ()
main = do
  mpath <- lookupEnv "ACE_MAP"
  getArgs >>= \s ->
    case s of
      players@(_:_:_)->
        Server.run mpath players
      _ -> do
        IO.hPutStr IO.stderr "usage: server <player-one-executable> <player-two-executable> ..."
        exitFailure
