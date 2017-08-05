{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ace.Serial (
    fromSiteId
  , toSiteId
  , fromSite
  , toSite
  , fromSites
  , toSites
  , fromMines
  , toMines
  , fromPunter
  , toPunter
  , fromMe
  , toMe
  , fromYou
  , toYou
  , fromPunterId
  , toPunterId
  , fromPunterCount
  , toPunterCount
  , fromMove
  , toMove
  , fromMoves
  , toMoves
  , fromRiver
  , toRiver
  , fromRivers
  , toRivers
  , fromWorld
  , toWorld
  , fromSetup
  , toSetup
  , fromScore
  , toScore
  , fromPunterScore
  , toPunterScore
  , fromPunterScores
  , toPunterScores
  , fromStop
  , toStop
  , fromMovesOrStop
  , toMovesOrStop
  , toRequest
  , fromState
  , toState
  , fromSetupResult
  , toSetupResult
  , fromMoveResult
  , toMoveResult
  , fromMoveResultServer
  , toMoveResultServer
  , fromMoveRequestServer
  , toMoveRequestServer
  , fromSettings
  , toSettings
  , asWith
  , as
  , packet
  ) where

import           Ace.Data

import           Data.Aeson (Value (..), toJSON, parseJSON, encode)
import           Data.Aeson (object, (.=), (.:), (.:?), withObject, eitherDecodeStrict)
import           Data.Aeson.Types (Parser, Result(..), parse)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Boxed
import qualified Data.Vector.Generic as Generic
import qualified Data.Vector.Unboxed as Unboxed

import           P


fromSiteId :: SiteId -> Value
fromSiteId =
  toJSON . siteId

toSiteId :: Value -> Parser SiteId
toSiteId =
  fmap SiteId . parseJSON

fromSite :: SiteId -> Value
fromSite s =
  object ["id" .= (toJSON . siteId) s]

toSite :: Value -> Parser SiteId
toSite =
  withObject "SiteId" $ \o ->
    SiteId <$> o .: "id"

fromSites :: Unboxed.Vector SiteId -> Value
fromSites =
  toJSON . fmap fromSite . box

toSites :: Value -> Parser (Unboxed.Vector SiteId)
toSites v =
  ((parseJSON v) :: Parser (Boxed.Vector Value)) >>= mapM toSite >>= pure . Unboxed.convert

fromMines :: Unboxed.Vector SiteId -> Value
fromMines =
  toJSON . fmap fromSiteId . box

toMines :: Value -> Parser (Unboxed.Vector SiteId)
toMines v =
  ((parseJSON v) :: Parser (Boxed.Vector Value)) >>= mapM toSiteId >>= pure . Unboxed.convert

fromPunter :: Punter -> Value
fromPunter =
  toJSON . renderPunter

toPunter :: Value -> Parser Punter
toPunter v =
  Punter <$> parseJSON v

fromMe :: (a -> Value) -> a -> Value
fromMe f a =
  toJSON . object $ ["me" .= f a]

toMe :: (Value -> Parser a) -> Value -> Parser a
toMe p =
  withObject "me" $ \o ->
    o .: "me" >>= p

fromYou :: (a -> Value) -> a -> Value
fromYou f a =
  toJSON . object $ ["you" .= f a]

toYou :: (Value -> Parser a) -> Value -> Parser a
toYou p =
  withObject "you" $ \o ->
    o .: "you" >>= p

fromPunterId :: PunterId -> Value
fromPunterId =
  toJSON . punterId

toPunterId :: Value -> Parser PunterId
toPunterId v =
  PunterId <$> parseJSON v

fromPunterCount :: PunterCount -> Value
fromPunterCount =
  toJSON . punterCount

toPunterCount :: Value -> Parser PunterCount
toPunterCount v =
  PunterCount <$> parseJSON v

fromMove :: Move -> Value
fromMove m =
  case m of
    Pass p ->
      object [
          "pass" .= object [
               "punter" .= fromPunterId p
             ]
        ]
    Claim p r ->
      object [
          "claim" .= object [
               "punter" .= fromPunterId p
             , "source" .= fromSiteId (riverSource r)
             , "target" .= fromSiteId (riverTarget r)
             ]
        ]

toMove :: Value -> Parser Move
toMove v =
  toClaim v <|> toPass v

toClaim :: Value -> Parser Move
toClaim =
  withObject "Move" $ \o ->
    o .: "claim" >>= (withObject "Claim" $ \c ->
      Claim
        <$> (c .: "punter" >>= toPunterId)
        <*> (makeRiver <$> (c .: "source" >>= toSiteId) <*> (c .: "target" >>= toSiteId)))

toPass :: Value -> Parser Move
toPass =
  withObject "Move" $ \o ->
    o .: "pass" >>= (withObject "Pass" $ \c ->
      Pass
        <$> (c .: "punter" >>= toPunterId))

fromMoves :: [Move] -> Value
fromMoves mvs =
  object [
    "move" .= object [
          "moves" .= toJSON (fmap fromMove mvs)
        ]
      ]

toMoves :: Value -> Parser [Move]
toMoves =
  withObject "[Move]" $ \o ->
    o .: "move" >>= (withObject "[Move]" $ \m ->
      m .: "moves" >>= mapM toMove)

fromMovesOrStop :: (a -> Value) -> MovesOrStop a -> Value
fromMovesOrStop from v =
  case v of
    JustMoves x ->
      fromMoves x
    JustStop x ->
      fromStop from x

toMovesOrStop :: (Value -> Parser a) -> Value -> Parser (MovesOrStop a)
toMovesOrStop to v =
  (JustStop <$> toStop to v) <|> (JustMoves <$> toMoves v)

fromRiver :: River -> Value
fromRiver r =
  object [
      "source" .= (fromSiteId . riverSource) r
    , "target" .= (fromSiteId . riverTarget) r
    ]

toRiver :: Value -> Parser River
toRiver =
  withObject "River" $ \o ->
    makeRiver
      <$> (o .: "source" >>= toSiteId)
      <*> (o .: "target" >>= toSiteId)

fromRivers :: Unboxed.Vector River -> Value
fromRivers =
  toJSON . fmap fromRiver . box

toRivers :: Value -> Parser (Unboxed.Vector River)
toRivers v =
  ((parseJSON v) :: Parser (Boxed.Vector Value)) >>= mapM toRiver >>= pure . Unboxed.convert

fromWorld :: World -> Value
fromWorld w =
  object [
      "sites" .= (fromSites . worldSites) w
    , "rivers" .= (fromRivers . worldRivers) w
    , "mines" .= (fromMines . worldMines) w
    ]

toWorld :: Value -> Parser World
toWorld =
  withObject "World" $ \o ->
    World
      <$> (o .: "sites" >>= toSites)
      <*> (o .: "mines" >>= toMines)
      <*> (o .: "rivers" >>= toRivers)

fromSetup :: Setup -> Value
fromSetup w =
  object [
      "punter" .= (fromPunterId . setupPunter) w
    , "punters" .= (fromPunterCount . setupPunterCount) w
    , "map" .= (fromWorld . setupWorld) w
    , "settings" .= (fromSettings . setupSettings) w
    ]

toSetup :: Value -> Parser Setup
toSetup =
  withObject "Setup" $ \o ->
    Setup
      <$> (o .: "punter" >>= toPunterId)
      <*> (o .: "punters" >>= toPunterCount)
      <*> (o .: "map" >>= toWorld)
      <*> (o .:? "settings" >>= maybe (pure defaultSettings) toSettings)

fromScore :: Score -> Value
fromScore =
  toJSON . score

toScore :: Value -> Parser Score
toScore v =
  Score <$> parseJSON v

fromPunterScore :: PunterScore -> Value
fromPunterScore s =
  object [
      "punter" .= (fromPunterId . scorePunter) s
    , "score" .= (fromScore . scoreValue) s
    ]

toPunterScore :: Value -> Parser PunterScore
toPunterScore =
  withObject "Score" $ \o ->
    PunterScore
      <$> (o .: "punter" >>= toPunterId)
      <*> (o .: "score" >>= toScore)

fromPunterScores :: [PunterScore] -> Value
fromPunterScores =
  toJSON . fmap fromPunterScore

toPunterScores :: Value -> Parser [PunterScore]
toPunterScores v =
  parseJSON v >>= mapM toPunterScore

fromStop :: (a -> Value) -> Stop a -> Value
fromStop from s =
  object [
      "stop" .=
        object [
            "moves" .= (fmap fromMove . stopMoves) s
          , "scores" .= (fromPunterScores . stopScores) s
          ]
    , "state" .= fmap from (stopState s)
    ]

toStop :: (Value -> Parser a) -> Value -> Parser (Stop a)
toStop to =
  withObject "Stop" $ \oo -> do
    st <- oo .:? "state" >>= mapM to
    oo .: "stop" >>= withObject "Stop'" (\o ->
        Stop
          <$> (o .: "moves" >>= mapM toMove)
          <*> (o .: "scores" >>= mapM toPunterScore)
          <*> pure st)

toRequest :: (Value -> Parser a) -> Value -> Parser (OfflineRequest a)
toRequest to v =
      (OfflineSetup <$> toSetup v)
  <|> (OfflineGameplay <$> (Gameplay <$> toMoves v) <*> (flip (withObject "state") v $ \o -> o .: "state" >>= toState to))
  <|> (OfflineScoring <$> toStop (toState to) v <*> (flip (withObject "state") v $ \o -> o .: "state" >>= toState to))

fromState :: (a -> Value) -> State a -> Value
fromState from (State p c w s a) =
  object [
      "punter" .= fromPunterId p
    , "count" .= fromPunterCount c
    , "world" .= fromWorld w
    , "settings" .= fromSettings s
    , "data" .= from a
    ]

toState :: (Value -> Parser a) -> Value -> Parser (State a)
toState to =
  withObject "State" $ \o ->
    State
      <$> (o .: "punter" >>= toPunterId)
      <*> (o .: "count" >>= toPunterCount)
      <*> (o .: "world" >>= toWorld)
      <*> (o .: "settings" >>= toSettings)
      <*> (o .: "data" >>= to)

fromSetupResult :: (a -> Value) -> SetupResult a -> Value
fromSetupResult from (SetupResult p s) =
  object [
      "ready" .= fromPunterId p
    , "state" .= from s
    ]

toSetupResult :: (Value -> Parser a) -> Value -> Parser (SetupResult a)
toSetupResult to =
  withObject "SetupResult" $ \o -> do
    SetupResult
      <$> (o .: "ready" >>= toPunterId)
      <*> (o .: "state" >>= to)

fromMoveResult :: (a -> Value) -> MoveResult a -> Value
fromMoveResult from (MoveResult m s) =
  object [
      "move" .= fromMove m
    , "state" .= fromState from s
    ]

toMoveResult :: (Value -> Parser a) -> Value -> Parser (MoveResult a)
toMoveResult to =
  withObject "MoveResult" $ \o -> do
    MoveResult
      <$> (o .: "move" >>= toMove)
      <*> (o .: "state" >>= toState to)

fromMoveResultServer :: (a -> Value) -> MoveResultServer a -> Value
fromMoveResultServer from (MoveResultServer m s) =
  object [
      "move" .= fromMove m
    , "state" .= from s
    ]

toMoveResultServer :: (Value -> Parser a) -> Value -> Parser (MoveResultServer a)
toMoveResultServer to =
  withObject "MoveResultServer" $ \o -> do
    MoveResultServer
      <$> (o .: "move" >>= toMove)
      <*> (o .: "state" >>= to)

fromMoveRequestServer :: (a -> Value) -> MoveRequestServer a -> Value
fromMoveRequestServer from (MoveRequestServer ms s) =
  object [
      "move" .= object [
          "moves" .= toJSON (fmap fromMove ms)
        ]
    , "state" .= from s
    ]

toMoveRequestServer :: (Value -> Parser a) -> Value -> Parser (MoveRequestServer a)
toMoveRequestServer to =
  withObject "MoveRequestServer" $ \o -> do
    MoveRequestServer
      <$> (o .: "move" >>= (withObject "[Move]" $ \m -> m .: "moves" >>= mapM toMove))
      <*> (o .: "state" >>= to)

fromSettings :: Settings -> Value
fromSettings s =
  object [
      "futures" .= (futuresSettings s == FuturesEnabled)
    ]

toSettings :: Value -> Parser Settings
toSettings =
  withObject "Settings" $ \o ->
    Settings
      <$> (o .:? "futures" >>= pure . maybe FuturesDisabled (bool FuturesDisabled FuturesEnabled))

box :: Generic.Vector v a => v a -> Boxed.Vector a
box =
  Unboxed.convert

asWith :: (Value -> Parser a) -> ByteString -> Either Text a
asWith to bs =
  (either (Left . Text.pack) Right . eitherDecodeStrict) bs >>= \a' -> case parse to a' of
    Success a -> pure a
    Error msg -> Left . Text.pack $ msg

as :: (a -> Value) -> a -> ByteString
as from =
  Lazy.toStrict . encode . from

packet :: Value -> ByteString
packet v =
  let
    j = Lazy.toStrict $ encode v
    l = ByteString.length j
  in
    (Text.encodeUtf8 . Text.pack . show $ l) <> ":" <> j
