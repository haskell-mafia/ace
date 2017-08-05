{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Ace.Data as Ace
import qualified Ace.Robot.Charles as Robot
import qualified Ace.Robot.Ibis as Robot
import qualified Ace.Robot.Lannister as Robot
import qualified Ace.Robot.Random as Robot
import qualified Ace.Online as Online

import qualified Data.Text as Text

import           P

import           System.IO (IO)
import           System.Environment (getArgs)
import           System.Exit (exitFailure)

main :: IO ()
main =
  getArgs >>= \s ->
    case s of
      h : pt : pn : robot : [] ->
        let
          run x =
            Online.run
              (Hostname $ Text.pack h)
              (Port . fromMaybe 0 $ readMaybe pt)
              (Punter $ Text.pack pn)
              x
        in
          case robot of
            "charles" ->
              run Robot.charles
            "cersei" ->
              run $ Robot.lannister Robot.Cersei
            "tyrion" ->
              run $ Robot.lannister Robot.Tyrion
            "lannister" ->
              run $ Robot.lannister Robot.Tyrion
            "random" ->
              run Robot.random
            "ibis" ->
              run Robot.ibis
            _ ->
              run Robot.random
      _ ->
        exitFailure
