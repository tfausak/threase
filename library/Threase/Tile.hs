-- | Exports Tile data type and functions for manipulating it.
module Threase.Tile (Tile (..), add, canAdd) where

-- | Represents a tile on the board.
data Tile = Tile
    { value :: Int -- ^ The number on the face of the tile.
    }

-- | Adds two tiles together.
add :: Tile -- ^ The first tile.
    -> Tile -- ^ The second tile.
    -> Tile -- ^ The new tile.
add (Tile a) (Tile b) = Tile (a + b)

-- | Tells you if two tiles can be added together.
canAdd :: Tile -- ^ The first tile.
    -> Tile -- ^ The second tile.
    -> Bool -- ^ Can the two tiles be added?
canAdd (Tile a) (Tile b) =
    (a == 1 && b == 2) ||
    (a == 2 && b == 1) ||
    (a /= 1 && a /= 2 && a == b)
