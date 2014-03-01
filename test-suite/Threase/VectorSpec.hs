module Threase.VectorSpec (spec) where

import           Test.Hspec
import           Threase.Tile   (Tile (..))
import           Threase.Vector

spec :: Spec
spec = do
    describe "squish" $ do
        it "does nothing to an empty vector" $ do
            let v = Vector [Nothing, Nothing, Nothing, Nothing]
            squish v `shouldBe` v

        it "does nothing to tiles already at the head" $ do
            let t = Just (Tile 1)
                v = Vector [t, Nothing, Nothing, Nothing]
            squish v `shouldBe` v

        it "moves tiles toward the head" $ do
            let t = Just (Tile 1)
                v = Vector [Nothing, Nothing, Nothing, t]
                v' = Vector [Nothing, Nothing, t, Nothing]
            squish v `shouldBe` v'

        it "does not add tiles that cannot be added" $ do
            let t = Just (Tile 1)
                v = Vector [t, t, t, t]
            squish v `shouldBe` v

        it "adds tiles that can be added" $ do
            let t = Just (Tile 3)
                v = Vector [t, t, Nothing, Nothing]
                t' = Just (Tile 6)
                v' = Vector [t', Nothing, Nothing, Nothing]
            squish v `shouldBe` v'

        it "does not add tiles if they can be moved" $ do
            let t = Just (Tile 3)
                v = Vector [Nothing, Nothing, t, t]
                v' = Vector [Nothing, t, t, Nothing]
            squish v `shouldBe` v'

        it "only adds one pair of tiles" $ do
            let t = Just (Tile 3)
                v = Vector [t, t, t, t]
                t' = Just (Tile 6)
                v' = Vector [t', t, t, Nothing]
            squish v `shouldBe` v'
