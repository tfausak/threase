{- |
    Tiles are the basis for the rest of the game. They can be added together
    (see 'add' and 'canAdd'), scored ('score'), and rendered ('render'). A
    typical Threes board has 16 tiles.
-}
module Threase.Tile (Tile (..), add, canAdd, render, score) where

{- |
    A single tile. In the game they have a lot of properties, including color,
    name, number, and some animations. We're only concerned with the number.
-}
data Tile = Tile
    { value :: Int -- ^ The tile's number.
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
add :: Tile -> Tile -> Tile
add (Tile a) (Tile b) = Tile (a + b)

{- |
    Determines if two tiles can be added.

    >>> canAdd (Tile 1) (Tile 2)
    True
-}
canAdd :: Tile -> Tile -> Bool
canAdd (Tile a) (Tile b) =
    (a == 1 && b == 2) ||
    (a == 2 && b == 1) ||
    (a > 2 && a == b)

{- |
    Renders a tile.

    >>> render (Tile 1)
    "1"
-}
render :: Tile -> String
render = show . value

{- |
    Calculates a tile's score.

    >>> score (Tile 6)
    9
-}
score :: Tile -> Int
score (Tile n) = if n < 3
    then 0
    else 3 ^ (n `div` 3)
