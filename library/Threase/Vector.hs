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

{- |
    Determines if a vector can be shifted.

    >>> canShift (Vector [Nothing, Just (T.Tile 3)])
    True
-}
canShift :: Vector -- ^ The vector.
    -> Bool -- ^ Can it be shifted?
canShift = go . tiles
  where
    go (Nothing : _) = True
    go (Just a : b'@(Just b) : rest) =
        T.canAdd a b || canShift (Vector (b' : rest))
    go _ = False

{- |
    Renders a vector.

    >>> render (Vector [Nothing, Just (T.Tile 3)])
    "-\t3"
-}
render :: Vector -- ^ The vector.
    -> String -- ^ A human-readable representation.
render = intercalate "\t" . fmap (maybe "-" T.render) . tiles

{- |
    Calculates a vector's score, which is the sum of the scores of its tiles.

    >>> score (Vector [Nothing, Just (T.Tile 6)])
    9
-}
score :: Vector -- ^ The input vector.
    -> Int -- ^ The vector's score.
score = sum . fmap T.score . catMaybes . tiles

{- |
    Moves the tiles in a vector toward the head. Keeps the size of the vector
    constant by filling in the last element with @Nothing@.

    >>> shift (Vector [Nothing, Just (T.Tile 3)])
    Vector {tiles = [Just (Tile {value = 3}),Nothing]}

    If the vector can't be shifted (i.e., 'canShift' is @False@), this just
    returns the vector.

    >>> shift (Vector [Just (T.Tile 3)])
    Vector {tiles = [Just (Tile {value = 3})]}

    This will add tiles if they can be added.

    >>> shift (Vector [Just (T.Tile 1), Just (T.Tile 2)])
    Vector {tiles = [Just (Tile {value = 3}),Nothing]}
-}
shift :: Vector -- ^ The vector.
    -> Vector -- ^ The shifted vector.
shift v = go (tiles v)
  where
    go (Nothing : rest) = Vector (rest <> [Nothing])
    go (Just a : Just b : rest) = if T.canAdd a b
        then Vector (Just (T.add a b) : rest <> [Nothing])
        else v
    go _ = v
