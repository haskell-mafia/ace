{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Ace.IO.Offline.Server as Server

import           P

import           System.IO (IO)
import qualified System.IO as IO
import           System.Environment (getArgs)
import           System.Exit (exitFailure)

main :: IO ()
main =
  getArgs >>= \s ->
    case s of
      players@(_:_:_)->
        Server.run players
      _ -> do
        IO.hPutStr IO.stderr "usage: server <player-one-executable> <player-two-executable> ..."
        exitFailure
