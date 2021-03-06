{- |
    The game itself. Well, a snapshot of one turn of the state, at least. Games
    have other properties that we don't (yet) care about, including number of
    turns and the hint about the next tile.
-}
module Threase.Game
    ( Game (..)
    , quality
    , render
    ) where

import           Data.List         (group, sort)
import           Data.Maybe        (catMaybes)
import           Data.Monoid       ((<>))
import qualified Threase.Board     as B
import qualified Threase.Direction as D
import qualified Threase.Tile      as T
import qualified Threase.Vector    as V

{- $setup
    >>> :{
        let game = Game
            { board = B.Board
                [ V.Vector [Nothing, Just (T.Tile 3)]
                , V.Vector [Just (T.Tile 1), Just (T.Tile 2)]
                ]
            , next = T.Tile 1
            }
    :}
-}

{- |
    A game. Just a wrapper around the board to provide game logic.

    >>> game -- Used in examples but annoying to type.
    Game {board = Board {vectors = [Vector {tiles = [Nothing,Just (Tile {number = 3})]},Vector {tiles = [Just (Tile {number = 1}),Just (Tile {number = 2})]}]}, next = Tile {number = 1}}
-}
data Game = Game
    { board :: B.Board -- ^ The game's board.
    , next  :: T.Tile -- ^ The next tile.
    } deriving (Eq, Show)

{- |
    Renders a game.

    >>> render game
    "Score: 3\nNext: 1\n-\t3\n1\t2\nMoves: \8592 \8594 \8593\nQuality: 10\n"
-}
render :: Game -> String
render g = unlines
    [ "Score: " <> show (score g)
    , "Next: " <> T.render (next g)
    , init (B.render (board g))
    , "Moves: " <> unwords (fmap D.render (moves g))
    , "Quality: " <> show (quality g)
    ]

{- |
    Determines the quality of a game. The quality is an arbitrary, subjective
    heuristic. It considers the following properties:

    * Score: Higher scores are better.
    * Available moves: Being able to move in every direction is better than not
    being able to move at all.
    * Duplicate tiles: More is worse.

    The quality should be only be used to compare the relative quality of
    games.

    >>> quality game > 0
    True
-}
quality :: Game -> Int
quality g
    | B.isOver (board g) = 0
    | otherwise = sum
        [ 1 * score g
        , 1 * numMoves g
        , 1 * (numTiles g - numDuplicates g)
        ]

moves :: Game -> [D.Direction]
moves g = filter (B.canShift . B.rotateTo b) ds
  where
    b = board g
    ds = [minBound :: D.Direction ..]

numDuplicates :: Game -> Int
numDuplicates g = length (filter p (group ns))
  where
    p = (> 1) . length
    ns = sort (fmap T.number ts)
    ts = catMaybes (tiles g)

numMoves :: Game -> Int
numMoves = length . moves

numTiles :: Game -> Int
numTiles = length . tiles

score :: Game -> Int
score = B.score . board

tiles :: Game -> [Maybe T.Tile]
tiles = (V.tiles =<<) . B.vectors . board
