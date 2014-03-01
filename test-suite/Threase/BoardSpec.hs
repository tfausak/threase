module Threase.BoardSpec (spec) where

import           Test.Hspec
import           Threase.Board
import           Threase.Tile   (Tile (..))
import           Threase.Vector (Vector (..))

spec :: Spec
spec = do
    describe "canShift" $ do
        it "returns True for an empty board" $ do
            let t = Nothing
                v = Vector (replicate 4 t)
                b = Board (replicate 4 v)
            canShift b `shouldBe` True

        it "returns False if none of the vectors can be shifted" $ do
            let n = Nothing
                t = Just (Tile 1)
                v = Vector [t, n, n, n]
                b = Board (replicate 4 v)
            canShift b `shouldBe` False

        it "returns True if any of the vectors can be shifted" $ do
            let n = Nothing
                t = Just (Tile 1)
                v = Vector [t, n, n, n]
                v' = Vector [n, t, n, n]
                b = Board [v, v, v, v']
            canShift b `shouldBe` True

    describe "score" $ do
        it "returns 0 for an empty board" $ do
            let t = Nothing
                v = Vector (replicate 4 t)
                b = Board (replicate 4 v)
            score b `shouldBe` 0

        it "returns the sum of the scores of the vectors" $ do
            let t = Just (Tile 3)
                v = Vector (replicate 4 t)
                b = Board (replicate 4 v)
            score b `shouldBe` 48

    describe "shift" $ do
        it "returns itself for an empty board" $ do
            let t = Nothing
                v = Vector (replicate 4 t)
                b = Board (replicate 4 v)
            shift b `shouldBe` b

        it "returns a board with all the vectors shifted" $ do
            let n = Nothing
                t = Just (Tile 3)
                b = Board
                    [ Vector [n, n, n, n]
                    , Vector [t, n, n, n]
                    , Vector [n, n, n, t]
                    , Vector [t, t, t, t]
                    ]
                t' = Just (Tile 6)
                b' = Board
                    [ Vector [n, n, n, n]
                    , Vector [t, n, n, n]
                    , Vector [n, n, t, n]
                    , Vector [t', t, t, n]
                    ]
            shift b `shouldBe` b'
