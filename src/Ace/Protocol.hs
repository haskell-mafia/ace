{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Ace.Protocol (
    readLength
  , readLength'
  , readMessage'
  ) where

import           Data.ByteString (ByteString, hGetSome, append)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lex.Integral as Lex

import           P

import           System.IO (IO, Handle)

readLength :: Handle -> IO (Maybe Int)
readLength h =
  readLength' (hGetSome h)

readLength' :: (Int -> IO ByteString) -> IO (Maybe Int)
readLength' inn =
  let
    go acc = do
      c <- inn 1
      if c == ":" then
        case Lex.readDecimal acc of
          Just (i :: Int, "") ->
            pure $! Just i
          _ ->
            pure Nothing
      else
       go $ acc `append` c
  in
    go ""

readMessage' :: (Int -> IO ByteString) -> IO (Maybe ByteString)
readMessage' inn = do
  l <- readLength' inn
  let
    go acc required = do
      xs <- inn required
      if ByteString.length xs == required
         then pure (Just $ acc <> xs)
         else if ByteString.null xs
           then pure Nothing
           else go (acc <> xs) (required - ByteString.length xs)
  maybe (pure Nothing) (go "") l
