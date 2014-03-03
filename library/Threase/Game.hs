{- |
    The game itself. Well, a snapshot of one turn of the state, at least. Games
    have other properties that we don't (yet) care about, including number of
    turns and the hint about the next tile.
-}
module Threase.Game (Game (..), isOver, quality) where

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

{- |
    Determines the quality of a game. The quality is an arbitrary, subjective
    heuristic. It considers the following properties:

    * Score: Higher scores are better.
    * Available moves: Being able to move in every direction is better than not
    being able to move at all.

    The quality will always be positive, but there is no limit. It should be
    used only to compare the relative quality of games.

    >>> let g1 = Game (B.Board [V.Vector [Nothing]])
    >>> let g2 = Game (B.Board [V.Vector [Just (T.Tile 3)]])
    >>> quality g2 > quality g1
    True
-}
quality :: Game -> Int
quality g = score + moves
  where
    b = board g
    score = B.score b
    moves = length (filter B.canShift (B.rotations b))
