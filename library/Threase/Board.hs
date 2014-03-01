-- | Types and tools for working with the entire Threes board.
module Threase.Board (Board (..), canShift, score, shift) where

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

-- | Calculate the score of a board by summing the scores of the vectors.
score :: Board -- ^ The board.
    -> Int -- ^ The score.
score = sum . fmap V.score . rows

-- | Shift all of a board's vectors.
shift :: Board -- ^ The input board.
    -> Board -- ^ The shifted board.
shift = Board . fmap V.shift . rows
