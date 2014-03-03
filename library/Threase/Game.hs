{- |
    The game itself. Well, a snapshot of one turn of the state, at least. Games
    have other properties that we don't (yet) care about, including number of
    turns and the hint about the next tile.
-}
module Threase.Game (Game (..), isOver) where

import qualified Threase.Board as B

{- $setup
    >>> import qualified Threase.Tile as T
    >>> import qualified Threase.Vector as V
    >>> :{
        let game = Game $ B.Board
            [ V.Vector [Nothing, Just (T.Tile 3)]
            , V.Vector [Just (T.Tile 1), Just (T.Tile 2)]
            ]
    :}
-}

{- |
    A game. Just a wrapper around the board to provide game logic.

    >>> game -- Used in examples but annoying to type.
    Game {board = Board {vectors = [Vector {tiles = [Nothing,Just (Tile {number = 3})]},Vector {tiles = [Just (Tile {number = 1}),Just (Tile {number = 2})]}]}}
-}
data Game = Game
    { board :: B.Board -- ^ The game's board.
    } deriving (Eq, Show)

{- |
    Determines if the game is over. The game is over if the board cannot be
    shifted in any direction.

    >>> isOver game
    False
-}
isOver :: Game -> Bool
isOver = not . any B.canShift . B.rotations . board
