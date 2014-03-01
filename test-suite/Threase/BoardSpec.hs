module Threase.BoardSpec (spec) where

import           Test.Hspec
import           Threase.Board
import           Threase.Tile   (Tile (..))
import           Threase.Vector (Vector (..))

spec :: Spec
spec = do
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
