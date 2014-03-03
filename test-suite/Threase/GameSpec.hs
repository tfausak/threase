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
