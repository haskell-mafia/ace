{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Ace.Analysis.Score where

import qualified Ace.Analysis.Score as Score
import           Ace.Data.Core
import qualified Ace.World.Generator as World

import qualified Data.Map.Strict as Map
import qualified Data.Vector.Unboxed as Unboxed

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           P

import           System.IO (IO)


genClaim :: World -> Gen Move
genClaim w =
  Claim <$> Gen.element (Unboxed.toList $ worldRivers w)

genMove :: World -> Gen Move
genMove w =
  Gen.frequency [
      (1, pure Pass)
    , (19, genClaim w)
    ]

genPunterCount :: Gen PunterCount
genPunterCount =
  PunterCount <$> Gen.int (Range.linear 2 16)

genPunterId :: PunterCount -> Gen PunterId
genPunterId n =
  PunterId <$> Gen.int (Range.constant 0 (punterCount n - 1))

genPunterMove :: World -> PunterCount -> Gen PunterMove
genPunterMove w n =
  PunterMove
    <$> genPunterId n
    <*> genMove w

prop_init :: Property
prop_init =
  property $ do
    n <- forAll genPunterCount
    world <- forAll $ World.genWorld_ 20

    for_ (punters n) $ \punter ->
      Score.score punter (Score.init world n) === 0

prop_one_v_all :: Property
prop_one_v_all =
  property $ do
    n <- forAll genPunterCount
    world <- forAll $ World.genWorld_ 20
    moves <- forAll $ Gen.list (Range.linear 0 100) (genPunterMove world n)

    let
      state0 =
        Score.init world n

      singleMoves =
        foldr Score.update state0 $ fmap pure moves

      allMoves =
        Score.update moves state0

    for_ (punters n) $ \punter ->
      Score.score punter singleMoves === Score.score punter allMoves

prop_punter_score_never_backwards :: Property
prop_punter_score_never_backwards =
  property $ do
    n <- forAll genPunterCount
    world <- forAll $ World.genWorld 1 20 3 10
    moves <- forAll $ Gen.list (Range.linear 0 100) (genPunterMove world n)

    let
      loop (state0, scores0) move = do
        let
          state =
            Score.update [move] state0

        scores <-
          for (punters n) $ \punter -> do
            let
              oldScore =
                fromMaybe 0 $ Map.lookup punter scores0

              newScore =
                Score.score punter state

            assert $ oldScore <= newScore

            pure (punter, newScore)

        pure (state, Map.fromList scores)

    foldM_ loop (Score.init world n, Map.empty) moves

tests :: IO Bool
tests =
  checkParallel $$(discover)
