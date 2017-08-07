{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Ace.IO.Offline.Server (
    ServerError (..)
  , renderServerError
  , run
  ) where

import qualified Ace.Analysis.Score as Score
import qualified Ace.Data.Binary as Binary
import           Ace.Data.Config
import           Ace.Data.Core
import           Ace.Data.Offline
import           Ace.Data.Protocol
import           Ace.Data.Robot
import           Ace.Data.Web
import           Ace.Serial
import qualified Ace.Web as Web

import           Control.Monad.IO.Class (liftIO)

import           Data.ByteString (ByteString)
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector.Unboxed as Unboxed


import           Text.Read (read)

import           P

import           System.IO (IO)
import qualified System.IO as IO
import qualified System.Process as Process

import           X.Control.Monad.Trans.Either (EitherT, hoistEither, left)


data ServerError =
    ServerParseError Text
  | ServerNoPlayers
    deriving (Eq, Show)

run :: GameId -> IO.FilePath -> [RobotIdentifier] -> World -> Config -> EitherT ServerError IO [PunterResult]
run gid executable robots world config = do
  liftIO $ Web.setup world gid
  let
    counter = PunterCount (length robots)
  initialised <- forM (List.zip robots [0..]) $ \(robot, n) ->
    setup executable robot (PunterId n) counter world config
  moves <- play (Unboxed.length . worldRivers $ world) gid config world initialised []

  let
    scores = scoreGame world (PunterCount $ length initialised) moves
  pure $ punterResults initialised scores

setup :: IO.FilePath -> RobotIdentifier -> PunterId -> PunterCount -> World -> Config -> EitherT ServerError IO Player
setup executable robot pid counter world config = do
  let
    player = Player executable robot pid (State pid . Binary.encode $ ()) 0
  r <- liftIO $ execute player  . packet . fromSetup $
    Setup pid counter world config
  v <- fmap setupResultState . hoistEither . first ServerParseError $
    asWith toSetupResult r
  pure $ player { playerState = v }

play :: Int -> GameId -> Config -> World -> [Player] -> [PunterMove] -> EitherT ServerError IO [PunterMove]
play n gid config world players last = do
  if n > 0
    then
      next n gid config world players last
    else
      stop gid world players last

scoreGame :: World -> PunterCount -> [PunterMove] -> [PunterScore]
scoreGame world pcount moves =
  let
    state =
      Score.update moves (Score.init pcount world)
  in
    with (punters pcount) $ \punter ->
      PunterScore punter (Score.score punter state)

stop :: GameId -> World -> [Player] -> [PunterMove] -> EitherT ServerError IO [PunterMove]
stop gid world players moves = do
  let
    scores = scoreGame world (PunterCount $ length players) moves
  liftIO $ Web.stop gid world players scores
  forM_ players $ \player ->
    liftIO $ execute player . packet . fromStop $ Stop moves scores (Just $ playerState player)
  pure moves

next :: Int -> GameId -> Config -> World -> [Player] -> [PunterMove] -> EitherT ServerError IO [PunterMove]
next n gid config world players last =
  case players of
    (x:xs) -> do
      r <- move x (List.take (List.length players) last)
      liftIO $ Web.move gid (moveResultMove r)
      let
        actualMove = moveResultMove r

        validatedMove =
          if validate config world last x actualMove then
            actualMove
          else
            actualMove { punterMoveValue = Pass }

        newBudget =
          if isPass . punterMoveValue $ validatedMove then
            playerSplurgeBudget x + 1
          else if isSplurge . punterMoveValue $ validatedMove then
            playerSplurgeBudget x - (length . moveRivers . punterMoveValue) validatedMove
          else
            playerSplurgeBudget x

      play (n - 1) gid config world (xs <> [x { playerState = moveResultState r, playerSplurgeBudget = newBudget }]) (moveResultMove r : last)
    [] ->
      left ServerNoPlayers

validate :: Config -> World -> [PunterMove] -> Player -> PunterMove -> Bool
validate config world moves player candidate =
  and [
      -- Only splurge when allowed
      splurgeConfig config == SplurgeEnabled || (not . isSplurge . punterMoveValue) candidate
      -- Only option when allowed
    , optionConfig config == OptionEnabled || (not . isOption . punterMoveValue) candidate
      -- Splurge must be within budget
    , (not . isSplurge . punterMoveValue) candidate || (length . moveRivers . punterMoveValue) candidate <= playerSplurgeBudget player
      -- Can't take an already claimed river unless options enabled and option rules followed.
    , case optionConfig config of
        OptionEnabled ->
          -- FIX need to fully validate option rules
          True
        OptionDisabled ->
          let
            claimed = moves >>= moveRivers . punterMoveValue
            requested = moveRivers . punterMoveValue $ candidate
            taken = requested `List.intersect` claimed
          in
            List.null taken
       -- World contains candidate rivers
    ,  and . with (moveRivers . punterMoveValue $ candidate) $ \r ->
         List.elem r $ (Unboxed.toList . worldRivers) world
    ]

move :: Player -> [PunterMove] -> EitherT ServerError IO MoveResult
move player last = do
  r <- liftIO $ execute player . packet . fromMoveRequestServer $ MoveRequestServer last (playerState player)
  hoistEither . first ServerParseError $
    asWith toMoveResult r

execute :: Player -> ByteString -> IO ByteString
execute player input = do
  let
    args = Text.unpack <$> [
        robotName . identifierName . playerRobot $ player
      , renderPunter . identifierPunter . playerRobot $ player
      ]
  output <- Process.readProcess (playerExecutable player) args (Text.unpack . Text.decodeUtf8 $ packet (fromYou fromPunter (identifierPunter . playerRobot $ player)) <> input)
  -- FIX shitty handshake
  let
    t = Text.pack output
    tt = Text.unpack . Text.takeWhile Char.isDigit $ t
    num = read tt
    rest = Text.drop (1 + num) . Text.dropWhile Char.isDigit $ t
  pure $ Text.encodeUtf8 . Text.drop 1 . Text.dropWhile (/= ':') $ rest



renderServerError :: ServerError -> Text
renderServerError err =
  case err of
    ServerParseError msg ->
      "Could not parse response: " <> msg
    ServerNoPlayers ->
      "No players."
