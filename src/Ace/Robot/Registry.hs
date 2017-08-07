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

import qualified Ace.Robot.Beaconsfield
import qualified Ace.Robot.Gold
import qualified Ace.Robot.Plastic
import qualified Ace.Robot.Lannister
import qualified Ace.Robot.Myopia
import qualified Ace.Robot.Silver

import qualified Data.List as List

import           P

primary :: Robot
primary =
  Ace.Robot.Silver.silver

robots :: [Robot]
robots = [
    Ace.Robot.Beaconsfield.beaconsfield 20 2 Ace.Robot.Gold.gold "beaconsfield"
  , Ace.Robot.Beaconsfield.beaconsfield 20 2 Ace.Robot.Gold.gold "beaconsfield-gold"
  , Ace.Robot.Beaconsfield.beaconsfield 20 2 Ace.Robot.Myopia.myopia "beaconsfield-myopia"
  , Ace.Robot.Beaconsfield.beaconsfield 20 2 Ace.Robot.Silver.silver "beaconsfield-silver"
  , Ace.Robot.Beaconsfield.beaconsfield 100 2 Ace.Robot.Gold.gold "beaconsfield-gold-really-high"
  , Ace.Robot.Beaconsfield.beaconsfield 40 2 Ace.Robot.Gold.gold "beaconsfield-gold-high"
  , Ace.Robot.Beaconsfield.beaconsfield 40 2 Ace.Robot.Silver.silver "beaconsfield-silver-high"
  , Ace.Robot.Beaconsfield.beaconsfield 20 8 Ace.Robot.Gold.gold "beaconsfield-gold-delayed"
  , Ace.Robot.Beaconsfield.beaconsfield 20 8 Ace.Robot.Silver.silver "beaconsfield-silver-delayed"
  , Ace.Robot.Lannister.lannister Ace.Robot.Lannister.Cersei
  , Ace.Robot.Silver.silver
  , Ace.Robot.Gold.gold
  , Ace.Robot.Myopia.myopia
  , Ace.Robot.Plastic.plastic
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
