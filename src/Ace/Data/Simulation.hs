{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ace.Data.Simulation (
    combinations
  ) where

import qualified Data.List as List

import           P

combinations :: Int -> [a] -> [[a]]
combinations k rivers0 =
  if k == 0 then
    [[]]
  else
    case rivers0 of
      [] ->
        []
      rs0 -> do
        x : rs <- List.tails rs0
        xs <- combinations (k - 1) rs
        pure $ x : xs
