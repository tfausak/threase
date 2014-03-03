{- |
    The possible directions to move in a game. This is tightly coupled to
    'Board.rotate'.
-}
module Threase.Direction (Direction (..)) where

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
