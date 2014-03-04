{- |
    The game itself. Well, a snapshot of one turn of the state, at least. Games
    have other properties that we don't (yet) care about, including number of
    turns and the hint about the next tile.
-}
module Threase.Game (Game (..), quality) where

import           Data.List      (genericLength, group, sort)
import           Data.Maybe     (catMaybes)
import qualified Threase.Board  as B
import qualified Threase.Tile   as T
import qualified Threase.Vector as V

{- $setup
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
    Determines the quality of a game. The quality is an arbitrary, subjective
    heuristic. It considers the following properties:

    * Score: Higher scores are better.
    * Available moves: Being able to move in every direction is better than not
    being able to move at all.
    * Duplicate tiles: More is worse.

    The quality should be only be used to compare the relative quality of
    games.

    >>> let g1 = Game (B.Board [V.Vector [Nothing]])
    >>> let g2 = Game (B.Board [V.Vector [Just (T.Tile 3)]])
    >>> quality g2 > quality g1
    True
-}
quality :: Game -> Integer
quality g = sum
    [ 1 * score
    , 1 * moves
    , 1 * (genericLength ns - duplicates)
    ]
  where
    b = board g
    score = B.score b
    moves = genericLength (filter B.canShift (B.rotations b))
    vs = B.vectors b
    ts = V.tiles =<< vs
    ns = fmap T.number (catMaybes ts)
    p = (> 1) . length
    duplicates = genericLength (filter p (group (sort ns)))
