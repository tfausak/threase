-- | Types and tools for working with linear arrays of tiles.
module Threase.Vector (Vector (..), canShift, render, score, shift) where

import           Data.List    (intercalate)
import           Data.Maybe   (catMaybes)
import           Data.Monoid  ((<>))
import qualified Threase.Tile as T

-- | A row or column of tiles.
data Vector = Vector
    { tiles :: [Maybe T.Tile] -- ^ The tiles in this row or column.
    } deriving (Eq, Show)

-- | Can a vector be shifted?
canShift :: Vector -- ^ The vector.
    -> Bool -- ^ Can it be shifted?
canShift = go . tiles
  where
    go (Nothing : _) = True
    go (Just a : b'@(Just b) : rest) =
        T.canAdd a b || canShift (Vector (b' : rest))
    go _ = False

-- | Render the vector in a human-readable format.
render :: Vector -- ^ The vector.
    -> String -- ^ A human-readable representation.
render = intercalate "\t" . fmap (maybe "-" T.render) . tiles

{- | Calculate the score for a vector. The score is the sum of the scores of
the tiles. -}
score :: Vector -- ^ The input vector.
    -> Int -- ^ The vector's score.
score = sum . fmap T.score . catMaybes . tiles

{- | Simulate a swipe and try to shift this vector. Moves tiles toward the head
of the list. -}
shift :: Vector -- ^ The vector.
    -> Vector -- ^ The shifted vector.
shift v = go (tiles v)
  where
    go (Nothing : rest) = Vector (rest <> [Nothing])
    go (Just a : Just b : rest) = if T.canAdd a b
        then Vector (Just (T.add a b) : rest <> [Nothing])
        else v
    go _ = v