{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Ace.Message (
    readLength
  ) where

import           Data.ByteString (hGetSome, append)
import qualified Data.ByteString.Lex.Integral as Lex

import           P

import           System.IO (IO, Handle)

readLength :: Handle -> IO (Maybe Int)
readLength inn =
  let
    go acc = do
      c <- hGetSome inn 1
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
