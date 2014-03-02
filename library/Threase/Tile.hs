-- | Exports Tile data type and functions for manipulating it.
module Threase.Tile (Tile (..), add, canAdd, render, score) where

-- | Represents a tile on the board.
data Tile = Tile
    { value :: Int -- ^ The number on the face of the tile.
    } deriving (Eq, Show)

{- |
    Adds two tiles.

    >>> add (Tile 1) (Tile 2)
    Tile {value = 3}

    Note that this will add them even if they can't be added (i.e., 'canAdd'
    is @False@).

    >>> let t = Tile 1
    >>> canAdd t t
    False
    >>> add t t
    Tile {value = 2}
-}
add :: Tile -- ^ The first tile.
    -> Tile -- ^ The second tile.
    -> Tile -- ^ The new tile.
add (Tile a) (Tile b) = Tile (a + b)

{- |
    Determines if two tiles can be added.

    >>> canAdd (Tile 1) (Tile 2)
    True
-}
canAdd :: Tile -- ^ The first tile.
    -> Tile -- ^ The second tile.
    -> Bool -- ^ Can the two tiles be added?
canAdd (Tile a) (Tile b) =
    (a == 1 && b == 2) ||
    (a == 2 && b == 1) ||
    (a > 2 && a == b)

{- |
    Renders a tile.

    >>> render (Tile 1)
    "1"
-}
render :: Tile -- ^ The tile.
    -> String -- ^ A human-readable string representation.
render = show . value

{- |
    Calculates a tile's score.

    >>> score (Tile 6)
    9
-}
score :: Tile -- ^ The tile.
    -> Int -- ^ The score.
score (Tile n) = if n < 3
    then 0
    else 3 ^ (n `div` 3)
