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
import           Ace.Robot.Compose (compose)

import qualified Ace.Robot.Beaconsfield
import qualified Ace.Robot.Carpe
import qualified Ace.Robot.Gold
import qualified Ace.Robot.Myopia
import qualified Ace.Robot.Silver

import qualified Data.List as List

import           P

primary :: Robot
primary = compose [
    Ace.Robot.Beaconsfield.beaconsfield 20 2 "beaconsfield"
  , Ace.Robot.Gold.gold
  ]

robots :: [Robot]
robots = [
    compose [
         Ace.Robot.Beaconsfield.beaconsfield 20 2 "beaconsfield"
       , Ace.Robot.Gold.gold
       ]
  , compose [
         Ace.Robot.Beaconsfield.beaconsfield 20 2 "beaconsfield"
       , Ace.Robot.Myopia.myopia
       ]
  , compose [
         Ace.Robot.Beaconsfield.beaconsfield 20 2 "beaconsfield"
       , Ace.Robot.Silver.silver
       ]
  , Ace.Robot.Silver.silver
  , Ace.Robot.Gold.gold
  , Ace.Robot.Myopia.myopia
  , compose [
        Ace.Robot.Carpe.carpe
      , Ace.Robot.Beaconsfield.beaconsfield 20 2 "beaconsfield"
      , Ace.Robot.Gold.gold
      ]
  , compose [
        Ace.Robot.Beaconsfield.beaconsfield 20 2 "beaconsfield"
      , Ace.Robot.Carpe.carpe
      , Ace.Robot.Gold.gold
      ]
  , compose [
        Ace.Robot.Carpe.carpe
      , Ace.Robot.Gold.gold
      ]
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
