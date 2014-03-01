-- | Types and tools for working with the entire Threes board.
module Threase.Board (Board (..), score) where

import qualified Threase.Vector as V

-- | A board of tiles.
data Board = Board
    { rows :: [V.Vector] -- ^ The rows of the board.
    } deriving (Eq, Show)

-- | Calculate the score of a board by summing the scores of the vectors.
score :: Board -- ^ The board.
    -> Int -- ^ The score.
score = sum . fmap V.score . rows
