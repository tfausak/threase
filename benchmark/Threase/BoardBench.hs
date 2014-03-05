module Threase.BoardBench (benchmarks) where

import           Criterion
import           Threase.Board
import           Threase.Direction (Direction (..))
import           Threase.Tile      (Tile (..))
import           Threase.Vector    (Vector (..))

benchmarks :: [Benchmark]
benchmarks =
    [ bench "canMove" $ whnf (canMove board) East
    , bench "canShift" $ whnf canShift board
    , bench "isOver" $ whnf isOver board
    , bench "move" $ whnf (move board) East
    , bench "render" $ whnf render board
    , bench "rotate" $ whnf rotate board
    , bench "rotateTo" $ whnf (rotateTo board) East
    , bench "rotations" $ whnf rotations board
    , bench "score" $ whnf score board
    , bench "shift" $ whnf shift board
    , bench "shiftWith" $ whnf shift board
    ]
  where
    board = Board
        [ Vector
            [ Nothing
            , Just (Tile 1)
            , Just (Tile 2)
            , Just (Tile 3)
            ]
        , Vector
            [ Just (Tile (3 * 2 ^ 1))
            , Just (Tile (3 * 2 ^ 2))
            , Just (Tile (3 * 2 ^ 3))
            , Just (Tile (3 * 2 ^ 4))
            ]
        , Vector
            [ Just (Tile (3 * 2 ^ 5))
            , Just (Tile (3 * 2 ^ 6))
            , Just (Tile (3 * 2 ^ 7))
            , Just (Tile (3 * 2 ^ 8))
            ]
        , Vector
            [ Just (Tile (3 * 2 ^ 9))
            , Just (Tile (3 * 2 ^ 10))
            , Just (Tile (3 * 2 ^ 11))
            , Nothing
            ]
        ]
