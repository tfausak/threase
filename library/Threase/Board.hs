{- |
    Boards are what you see when you play the game. A board can do anything a
    vector can do since it's just a collection of them. In the game, you can
    shift the board in four different directions. To achieve that, rotate the
    board first, then shift it.
-}
module Threase.Board (Board (..), canShift, render, rotations, score, shift) where

import           Data.List      (transpose)
import qualified Threase.Vector as V

{- $setup
    >>> import qualified Threase.Tile as T
    >>> :{
        let board = Board
            [ V.Vector [Nothing, Just (T.Tile 3)]
            , V.Vector [Just (T.Tile 1), Just (T.Tile 2)]
            ]
    :}
-}

{- |
    An entire game board. This is just a list of vectors. It's implied, but not
    enforced, that the board has the same number of rows and columns. In other
    words, it should be square.

    >>> board -- Used in examples but annoying to type.
    Board {vectors = [Vector {tiles = [Nothing,Just (Tile {number = 3})]},Vector {tiles = [Just (Tile {number = 1}),Just (Tile {number = 2})]}]}
-}
data Board = Board
    { vectors :: [V.Vector] -- ^ The board's vectors.
    } deriving (Eq, Show)

{- |
    Determines if a board can be shifted.

    >>> canShift board
    True
-}
canShift :: Board -> Bool
canShift = any V.canShift . vectors

{- |
    Renders a board.

    >>> render board
    "-\t3\n1\t2\n"
-}
render :: Board -> String
render = unlines . fmap V.render . vectors

{- |
    Generates rotated boards from a board.

    >>> map render (rotations board)
    ["-\t3\n1\t2\n","1\t-\n2\t3\n","2\t1\n3\t-\n","3\t2\n-\t1\n"]
-}
rotations :: Board -> [Board]
rotations = take 4 . iterate rotate
  where
    rotate = fromLists . fmap reverse . transpose . toLists
    toLists = fmap V.tiles . vectors
    fromLists = Board . fmap V.Vector

{- |
    Calculates the score of a board, which is the sum of the scores of its
    vectors.

    >>> score board
    3
-}
score :: Board -> Int
score = sum . fmap V.score . vectors

{- |
    Shifts all the vectors in a board.

    >>> render (shift board)
    "3\t-\n3\t-\n"
-}
shift :: Board -> Board
shift = Board . fmap V.shift . vectors
