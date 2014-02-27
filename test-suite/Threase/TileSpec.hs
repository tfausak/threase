module Threase.TileSpec (spec) where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Threase.Tile

spec :: Spec
spec = do
    describe "add" $ do
        it "returns 3 for 1 and 2" $ do
            let a = Tile 1
            let b = Tile 2
            value (add a b) `shouldBe` 3

        it "returns 6 for 3 and 3" $ do
            let a = Tile 3
            let b = Tile 3
            value (add a b) `shouldBe` 6

        prop "returns the sum of the values" $
            \ a b -> value (add (Tile a) (Tile b)) == a + b

    describe "canAdd" $ do
        it "returns True for 1 and 2" $ do
            let a = Tile 1
            let b = Tile 2
            canAdd a b `shouldBe` True

        it "returns False for 1 and 1" $ do
            let a = Tile 1
            let b = Tile 1
            canAdd a b `shouldBe` False

        it "returns False for 2 and 2" $ do
            let a = Tile 1
            let b = Tile 1
            canAdd a b `shouldBe` False

        it "returns True for 3 and 3" $ do
            let a = Tile 3
            let b = Tile 3
            canAdd a b `shouldBe` True

        prop "returns True for pairs other than 1 and 2" $
            \ n -> n == 1 || n == 2 || canAdd (Tile n) (Tile n)
