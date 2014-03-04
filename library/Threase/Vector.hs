{- |
    Vectors are the main interface for the game. Each vector can be considered
    independently. The can be rendered and scored ('render' and 'score',
    respectively) like tiles. They can also be shifted ('shift' and 'canShift')
    toward the head (left).
-}
module Threase.Vector (Vector (..), canShift, render, score, shift) where

import           Data.List    (intercalate)
import           Data.Maybe   (catMaybes)
import           Data.Monoid  ((<>))
import qualified Threase.Tile as T

{- |
    A single row or column. This is just a list of tiles. Unoccupied positions
    are represented by @Nothing@.
-}
data Vector = Vector
    { tiles :: [Maybe T.Tile] -- ^ The vector's tiles.
    } deriving (Eq, Show)

{- |
    Determines if a vector can be shifted.

    >>> canShift (Vector [Nothing, Just (T.Tile 3)])
    True
-}
canShift :: Vector -> Bool
canShift v = shift v /= v

{- |
    Renders a vector.

    >>> render (Vector [Nothing, Just (T.Tile 3)])
    "-\t3"
-}
render :: Vector -> String
render = intercalate "\t" . fmap (maybe "-" T.render) . tiles

{- |
    Calculates a vector's score, which is the sum of the scores of its tiles.

    >>> score (Vector [Nothing, Just (T.Tile 6)])
    9
-}
score :: Vector -> Integer
score = sum . fmap T.score . catMaybes . tiles

{- |
    Moves the tiles in a vector toward the head. Keeps the size of the vector
    constant by filling in the last element with @Nothing@.

    >>> shift (Vector [Nothing, Just (T.Tile 3)])
    Vector {tiles = [Just (Tile {number = 3}),Nothing]}

    If the vector can't be shifted (i.e., 'canShift' is @False@), this just
    returns the vector.

    >>> shift (Vector [Just (T.Tile 3)])
    Vector {tiles = [Just (Tile {number = 3})]}

    This will add tiles if they can be added.

    >>> shift (Vector [Just (T.Tile 1), Just (T.Tile 2)])
    Vector {tiles = [Just (Tile {number = 3}),Nothing]}
-}
shift :: Vector -> Vector
shift = Vector . go . tiles
  where
    go (Just a : Just b : ts) = if T.canAdd a b
        then Just (T.add a b) : ts <> [Nothing]
        else Just a : go (Just b : ts)
    go (Nothing : ts) = ts <> [Nothing]
    go (t : ts) = t : go ts
    go ts = ts
