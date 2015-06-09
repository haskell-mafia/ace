{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           BuildInfo_ambiata_ace
import           DependencyInfo_ambiata_ace

import           Options.Applicative

import           P

import           System.IO
import           System.Exit
import           X.Options.Applicative

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  cli "ace" buildInfoVersion dependencyInfo parser $ \cmd ->
    case cmd of
      Command ->
        putStrLn "*implement me*" >> exitFailure

parser :: Parser Command
parser =
  subparser $
    command' "ace" "*description of ace*"
      (pure Command)

data Command =
  Command
  deriving (Eq, Show)
