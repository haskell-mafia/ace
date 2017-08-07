{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Ace.Robot.Registry (
    robots
  , pick
  , names
  , primary
  ) where

import           Ace.Data.Robot

import qualified Ace.Robot.Charles
import qualified Ace.Robot.Gold
import qualified Ace.Robot.Lannister
import qualified Ace.Robot.Myopia
import qualified Ace.Robot.Random
import qualified Ace.Robot.Silver

import qualified Data.List as List

import           P

primary :: Robot
primary =
  Ace.Robot.Silver.silver

robots :: [Robot]
robots = [
    Ace.Robot.Charles.charles
  , Ace.Robot.Lannister.lannister Ace.Robot.Lannister.Cersei
  , Ace.Robot.Lannister.lannister Ace.Robot.Lannister.Tyrion
  , Ace.Robot.Random.random
  , Ace.Robot.Silver.silver
  , Ace.Robot.Gold.gold
  , Ace.Robot.Myopia.myopia
  ]

pick :: RobotName -> Maybe Robot
pick name =
  flip List.find robots $ \robot ->
    case robot of
      Robot label _ _ ->
        RobotName label == name

names :: [RobotName]
names =
  nameOf <$> robots
