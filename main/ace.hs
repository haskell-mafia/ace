{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Ace.Message (readLength)

import           Data.ByteString (ByteString, hGet, hPut)

import           P

import           System.IO (IO, BufferMode(..), Handle)
import           System.IO (hSetBuffering, stdin, stdout, stderr)
import           System.Exit (exitFailure)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  run stdin stdout

run :: Handle -> Handle -> IO ()
run inn out = do

  m <- readLength inn

  n <- case m of
    Just i ->
      pure i
    Nothing ->
      exitFailure

  rest <- hGet inn n

  result <- process rest

  hPut out result

process :: ByteString -> IO ByteString
process bs =
  pure bs
