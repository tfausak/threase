module Threase.GameSpec (spec) where

import           Test.Hspec
import           Threase.Board  (Board (..))
import           Threase.Game
import           Threase.Tile   (Tile (..))
import           Threase.Vector (Vector (..))

spec :: Spec
spec = do
    describe "quality" $ do
        it "returns 0 if the game is over" $ do
            let game = g 1
                    [ [1, 3, 1, 3]
                    , [3, 1, 3, 1]
                    , [1, 3, 1, 3]
                    , [3, 1, 3, 1]
                    ]
            quality game `shouldBe` 0

        it "considers the score" $ do
            let g1 = g 1
                    [ [1, 1, 1, 1]
                    , [1, 1, 1, 1]
                    , [1, 1, 1, 1]
                    , [1, 1, 1, 0]
                    ]
                g2 = g 1
                    [ [3, 1, 1, 1]
                    , [1, 1, 1, 1]
                    , [1, 1, 1, 1]
                    , [1, 1, 1, 0]
                    ]
            quality g2 `shouldSatisfy` (> quality g1)

        it "considers the available moves" $ do
            let g1 = g 1
                    [ [1, 1, 1, 1]
                    , [1, 1, 1, 1]
                    , [1, 1, 1, 1]
                    , [1, 1, 1, 0]
                    ]
                g2 = g 1
                    [ [0, 1, 1, 1]
                    , [1, 1, 1, 1]
                    , [1, 1, 1, 1]
                    , [1, 1, 1, 0]
                    ]
            quality g2 `shouldSatisfy` (> quality g1)

        it "considers duplicate tiles" $ do
            let g1 = g 1
                    [ [1, 0, 0, 0]
                    , [0, 1, 0, 0]
                    , [0, 0, 0, 0]
                    , [0, 0, 0, 0]
                    ]
                g2 = g 1
                    [ [1, 0, 0, 0]
                    , [0, 2, 0, 0]
                    , [0, 0, 0, 0]
                    , [0, 0, 0, 0]
                    ]
            quality g2 `shouldSatisfy` (> quality g1)

    describe "render" $ do
        it "returns the rendered board with metadata" $ do
            let game = g 24
                    [ [1, 0, 0, 0]
                    , [0, 2, 0, 0]
                    , [0, 0, 3, 0]
                    , [0, 0, 0, 6]
                    ]
            render game `shouldBe` unlines
                [ "Score: 12"
                , "Next: 24"
                , "1\t-\t-\t-"
                , "-\t2\t-\t-"
                , "-\t-\t3\t-"
                , "-\t-\t-\t6"
                , "Moves: \8592 \8595 \8594 \8593"
                , "Quality: 32"
                ]

t :: Int -> Tile
t = Tile

mt :: Int -> Maybe Tile
mt 0 = Nothing
mt n = Just (t n)

v :: [Int] -> Vector
v = Vector . fmap mt

b :: [[Int]] -> Board
b = Board . fmap v

g :: Int -> [[Int]] -> Game
g n ns = Game
    { board = b ns
    , next = t n
    }
