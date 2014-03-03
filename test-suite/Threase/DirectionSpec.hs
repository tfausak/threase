module Threase.DirectionSpec (spec) where

import           Test.Hspec
import           Threase.Direction

spec :: Spec
spec = do
    describe "render" $ do
        it "returns a left arrow for west" $ do
            render West `shouldBe` "\8592"

        it "returns a down arrow for south" $ do
            render South `shouldBe` "\8595"

        it "returns a right arrow for east" $ do
            render East `shouldBe` "\8594"

        it "returns a up arrow for north" $ do
            render North `shouldBe` "\8593"
