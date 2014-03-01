module Threase.VectorSpec (spec) where

import           Test.Hspec
import           Threase.Tile   (Tile (..))
import           Threase.Vector

spec :: Spec
spec = do
    describe "canShift" $ do
        it "returns true for an empty vector" $ do
            let v = Vector [Nothing, Nothing, Nothing, Nothing]
            canShift v `shouldBe` True

        it "returns false with a tile at the head" $ do
            let t = Just (Tile 1)
                v = Vector [t, Nothing, Nothing, Nothing]
            canShift v `shouldBe` False

        it "returns true with a tile not at the head" $ do
            let t = Just (Tile 1)
                v = Vector [Nothing, t, Nothing, Nothing]
            canShift v `shouldBe` True

        it "returns true with addable tiles" $ do
            let t = Just (Tile 3)
                v = Vector [t, t, Nothing, Nothing]
            canShift v `shouldBe` True

        it "returns true with addable tiles after unaddable ones" $ do
            let a = Just (Tile 1)
                b = Just (Tile 2)
                v = Vector [a, a, b, Nothing]
            canShift v `shouldBe` True

    describe "score" $ do
        it "returns 0 for an empty vector" $ do
            let v = Vector [Nothing, Nothing, Nothing, Nothing]
            score v `shouldBe` 0

        it "returns the sum of the scores of the tiles" $ do
            let a = Just (Tile 1)
                b = Just (Tile 2)
                c = Just (Tile 3)
                d = Just (Tile 6)
                v = Vector [a, b, c, d]
            score v `shouldBe` 12

    describe "shift" $ do
        it "does nothing to an empty vector" $ do
            let v = Vector [Nothing, Nothing, Nothing, Nothing]
            shift v `shouldBe` v

        it "does nothing to tiles already at the head" $ do
            let t = Just (Tile 1)
                v = Vector [t, Nothing, Nothing, Nothing]
            shift v `shouldBe` v

        it "moves tiles toward the head" $ do
            let t = Just (Tile 1)
                v = Vector [Nothing, Nothing, Nothing, t]
                v' = Vector [Nothing, Nothing, t, Nothing]
            shift v `shouldBe` v'

        it "does not add tiles that cannot be added" $ do
            let t = Just (Tile 1)
                v = Vector [t, t, t, t]
            shift v `shouldBe` v

        it "adds tiles that can be added" $ do
            let t = Just (Tile 3)
                v = Vector [t, t, Nothing, Nothing]
                t' = Just (Tile 6)
                v' = Vector [t', Nothing, Nothing, Nothing]
            shift v `shouldBe` v'

        it "does not add tiles if they can be moved" $ do
            let t = Just (Tile 3)
                v = Vector [Nothing, Nothing, t, t]
                v' = Vector [Nothing, t, t, Nothing]
            shift v `shouldBe` v'

        it "only adds one pair of tiles" $ do
            let t = Just (Tile 3)
                v = Vector [t, t, t, t]
                t' = Just (Tile 6)
                v' = Vector [t', t, t, Nothing]
            shift v `shouldBe` v'
