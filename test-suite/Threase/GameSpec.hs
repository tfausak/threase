module Threase.GameSpec (spec) where

import           Test.Hspec
import           Threase.Board  (Board (..))
import           Threase.Game
import           Threase.Tile   (Tile (..))
import           Threase.Vector (Vector (..))

spec :: Spec
spec = do
    describe "quality" $ do
        it "considers the score" $ do
            let g1 = Game (Board [Vector [Nothing]])
                g2 = Game (Board [Vector [Just (Tile 3)]])
            quality g2 `shouldSatisfy` (> quality g1)

        it "considers the available moves" $ do
            let g1 = Game (Board [Vector [Nothing, Nothing]])
                g2 = Game (Board [Vector [Nothing, Just (Tile 1)]])
            quality g2 `shouldSatisfy` (> quality g1)
