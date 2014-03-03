module Threase.GameSpec (spec) where

import           Test.Hspec
import qualified Threase.Board  as B
import           Threase.Game
import qualified Threase.Tile   as T
import qualified Threase.Vector as V

spec :: Spec
spec = do
    describe "isOver" $ do
        it "returns True for an empty board" $ do
            let t = Nothing
                v = V.Vector (replicate 4 t)
                b = B.Board (replicate 4 v)
                g = Game b
            isOver g `shouldBe` True

        it "returns True if none of the rotations can be shifted" $ do
            let t = Just (T.Tile 1)
                v = V.Vector (replicate 4 t)
                b = B.Board (replicate 4 v)
                g = Game b
            isOver g `shouldBe` True

        it "returns False if any of the vectors can be shifted" $ do
            let n = Nothing
                t = Just (T.Tile 1)
                v = V.Vector [t, n, n, n]
                b = B.Board (replicate 4 v)
                g = Game b
            isOver g `shouldBe` False

    describe "quality" $ do
        it "considers the score" $ do
            let g1 = Game (B.Board [V.Vector [Nothing]])
                g2 = Game (B.Board [V.Vector [Just (T.Tile 3)]])
            quality g2 `shouldSatisfy` (> quality g1)

        it "considers the available moves" $ do
            let g1 = Game (B.Board [V.Vector [Nothing, Nothing]])
                g2 = Game (B.Board [V.Vector [Nothing, Just (T.Tile 1)]])
            quality g2 `shouldSatisfy` (> quality g1)
