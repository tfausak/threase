module Threase.GameSpec (spec) where

import           Test.Hspec
import qualified Threase.Board  as B
import           Threase.Game
import qualified Threase.Tile   as T
import qualified Threase.Vector as V

spec :: Spec
spec = do
    describe "quality" $ do
        it "considers the score" $ do
            let g1 = Game (B.Board [V.Vector [Nothing]])
                g2 = Game (B.Board [V.Vector [Just (T.Tile 3)]])
            quality g2 `shouldSatisfy` (> quality g1)

        it "considers the available moves" $ do
            let g1 = Game (B.Board [V.Vector [Nothing, Nothing]])
                g2 = Game (B.Board [V.Vector [Nothing, Just (T.Tile 1)]])
            quality g2 `shouldSatisfy` (> quality g1)
