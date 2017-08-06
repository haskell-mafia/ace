{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Ace.Data.Online
import           Ace.Data.Protocol
import qualified Ace.IO.Online as Online
import qualified Ace.Robot.Charles as Robot
import qualified Ace.Robot.Lannister as Robot
import qualified Ace.Robot.Random as Robot
import qualified Ace.Robot.Silver as Robot

import qualified Data.Text as Text

import           P

import           System.IO (IO)
import           System.Environment (getArgs)
import           System.Exit (exitFailure)

import           X.Control.Monad.Trans.Either.Exit (orDie)


main :: IO ()
main =
  getArgs >>= \s ->
    case s of
      h : pt : pn : robot : [] ->
        let
          run x =
            orDie Online.renderOnlineError $ Online.run
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
            "silver" ->
              run $ Robot.silver
            "random" ->
              run Robot.random
            _ ->
              run Robot.silver
      _ ->
        exitFailure
