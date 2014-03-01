module Threase.VectorSpec (spec) where

import           Test.Hspec
import           Threase.Tile   (Tile (..))
import           Threase.Vector

spec :: Spec
spec = do
    describe "score" $ do
        it "returns 0 form an empty vector" $ do
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
