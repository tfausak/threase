{- |
    The possible directions to move in a game. This is tightly coupled to
    'Board.rotate'.
-}
module Threase.Direction
    ( Direction (..)
    , render
    ) where

{- |
    A direction the board can be moved in. Represented by the cardinal
    directions instead of relative directions to avoid colliding with 'Left'
    and 'Right'.
-}
data Direction
    = West
    | South
    | East
    | North
    deriving (Bounded, Enum, Eq, Show)

{- |
    Renders a direction.

    >>> render West
    "\8592"
-}
render :: Direction -> String
render West = "\8592"
render South = "\8595"
render East = "\8594"
render North = "\8593"
