module Threase.TileSpec (spec) where

import           Test.Hspec
import           Test.Hspec.QuickCheck
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

        prop "returns the sum of the values" $
            \ a b -> add (Tile a) (Tile b) == Tile (a + b)

        prop "is commutative" $
            \ a b -> add (Tile a) (Tile b) == add (Tile b) (Tile a)

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

        prop "returns True for pairs other than 1 and 2" $
            \ n -> n < 3 || canAdd (Tile n) (Tile n)

        prop "is commutative" $
            \ a b -> canAdd (Tile a) (Tile b) == canAdd (Tile b) (Tile a)

    describe "render" $ do
        prop "returns the number as a string" $
            \ n -> render (Tile n) == show n

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

        prop "returns 3 ^ n for 3 * 2 ^ n" $ \ n ->
            n < 0 ||
            n > 11 || -- There are only 12 tiles.
            score (Tile (3 * 2 ^ n)) == 3 ^ (1 + n :: Int)

    describe "number" $ do
        prop "returns the tile's number" $
            \ n -> number (Tile n) == n
