module Threase.TileSpec (spec) where

import           Test.Hspec
import           Threase.Tile

spec :: Spec
spec = do
    describe "add" $ do
        it "returns 3 for 1 and 2" $ do
            let a = Tile 1
                b = Tile 2
            add a b `shouldBe` Tile 3

        it "returns 6 for 3 and 3" $ do
            let a = Tile 3
            add a a `shouldBe` Tile 6

    describe "canAdd" $ do
        it "returns True for 1 and 2" $ do
            let a = Tile 1
                b = Tile 2
            canAdd a b `shouldBe` True

        it "returns True for 2 and 1" $ do
            let a = Tile 2
                b = Tile 1
            canAdd a b `shouldBe` True

        it "returns False for 1 and 1" $ do
            let a = Tile 1
                b = Tile 1
            canAdd a b `shouldBe` False

        it "returns False for 2 and 2" $ do
            let a = Tile 1
                b = Tile 1
            canAdd a b `shouldBe` False

        it "returns True for 3 and 3" $ do
            let a = Tile 3
                b = Tile 3
            canAdd a b `shouldBe` True

    describe "render" $ do
        it "returns the number as a string" $ do
            render (Tile 1) `shouldBe` "1"

    describe "score" $ do
        it "returns 0 for 1" $ do
            score (Tile 1) `shouldBe` 0

        it "returns 0 for 2" $ do
            score (Tile 2) `shouldBe` 0

        it "returns 3 for 3" $ do
            score (Tile 3) `shouldBe` 3

        it "returns 9 for 6" $ do
            score (Tile 6) `shouldBe` 9

        it "returns 27 for 12" $ do
            score (Tile 12) `shouldBe` 27

    describe "number" $ do
        it "returns the tile's number" $ do
            number (Tile 1) `shouldBe` 1
