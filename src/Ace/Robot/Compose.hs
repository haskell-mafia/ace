{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Ace.Robot.Compose (
    Compose(..)
  , compose
  ) where

import qualified Ace.Data.Binary as Binary (decodeBytes)
import           Ace.Data.Config
import           Ace.Data.Core
import           Ace.Data.Robot

import qualified Data.Binary as Binary
import           Data.Binary (Binary)
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Text as Text
import qualified Data.Vector as Boxed

import           GHC.Generics (Generic)

import           P

import           System.IO (IO)


data Compose =
  Compose {
      composeRobotStates :: Boxed.Vector Lazy.ByteString
    } deriving (Eq, Show, Generic)

instance Binary Compose

instance Monoid Compose where
  mempty =
    Compose Boxed.empty
  mappend (Compose x) (Compose y) =
    Compose (x <> y)

compose :: [Robot] -> Robot
compose robots =
  Robot (Text.intercalate "-" . fmap robotLabel $ robots) (initAll robots) (moveAll $ Boxed.fromList robots)

initOne :: PunterId -> PunterCount -> World -> Config -> Robot -> IO (Initialisation Compose)
initOne punter pcount world config robot =
  case robot of
    Robot _ init _ ->
      fmap (Compose . Boxed.singleton . Binary.encode) <$> init punter pcount world config

initAll :: [Robot] -> PunterId -> PunterCount -> World -> Config -> IO (Initialisation Compose)
initAll robots punter pcount world config = do
  mconcat <$> mapM (initOne punter pcount world config) robots

moveOne :: [PunterMove] -> Robot -> Lazy.ByteString -> IO (RobotMove Lazy.ByteString)
moveOne moves robot blob0 =
  case robot of
    Robot _ _ move ->
      case Binary.decodeBytes blob0 of
        Left _ ->
          -- could not decode, give up on move
          pure $ RobotMove Nothing blob0
        Right state0 -> do
          rmove <- move moves state0
          pure $ fmap Binary.encode rmove

moveAll :: Boxed.Vector Robot -> [PunterMove] -> Compose -> IO (RobotMove Compose)
moveAll robots moves state = do
  let
    blobs =
      composeRobotStates state

  rmoves <- Boxed.zipWithM (moveOne moves) robots blobs

  let
    moveBest =
      asum $ fmap robotMoveValue rmoves

    rstates =
      fmap robotMoveState rmoves

  pure $ RobotMove moveBest (Compose rstates)
