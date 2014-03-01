-- | Types and tools for working with the entire Threes board.
module Threase.Board (Board (..), canShift, render, rotations, score, shift) where

import           Data.List      (transpose)
import qualified Threase.Vector as V

-- | A board of tiles.
data Board = Board
    { rows :: [V.Vector] -- ^ The rows of the board.
    } deriving (Eq, Show)

{- | Tells if the board can be shifted. A board can be shifted if any of its
vectors can be shifted. -}
canShift :: Board -- ^ The board.
    -> Bool -- ^ Can the board be shifted?
canShift = any V.canShift . rows

-- | Render the board in a human-readable format.
render :: Board -- ^ The board.
    -> String -- ^ A human-readable representation.
render = unlines . fmap V.render . rows

-- | Generate all the boards that can be made by rotating the board.
rotations :: Board -- ^ The input board.
    -> [Board] -- ^ The rotated boards.
rotations = take 4 . iterate rotate
  where
    rotate = fromLists . fmap reverse . transpose . toLists
    toLists = fmap V.tiles . rows
    fromLists = Board . fmap V.Vector

-- | Calculate the score of a board by summing the scores of the vectors.
score :: Board -- ^ The board.
    -> Int -- ^ The score.
score = sum . fmap V.score . rows

-- | Shift all of a board's vectors.
shift :: Board -- ^ The input board.
    -> Board -- ^ The shifted board.
shift = Board . fmap V.shift . rows