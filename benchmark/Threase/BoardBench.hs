module Threase.BoardBench (benchmarks) where

import           Criterion
import           Threase.Board
import           Threase.Direction
import qualified Threase.Tile      as T
import qualified Threase.Vector    as V

benchmarks :: [Benchmark]
benchmarks =
    [ bench "canShift" $ whnf canShift board
    , bench "move" $ whnf (move board) East
    , bench "render" $ whnf render board
    , bench "rotate" $ whnf rotate board
    , bench "rotations" $ whnf rotations board
    , bench "score" $ whnf score board
    , bench "shift" $ whnf shift board
    ]
  where
    board = Board
        [ V.Vector
            [ Nothing
            , Just (T.Tile 1)
            , Just (T.Tile 2)
            , Just (T.Tile 3)
            ]
        , V.Vector
            [ Just (T.Tile (3 * 2 ^ 1))
            , Just (T.Tile (3 * 2 ^ 2))
            , Just (T.Tile (3 * 2 ^ 3))
            , Just (T.Tile (3 * 2 ^ 4))
            ]
        , V.Vector
            [ Just (T.Tile (3 * 2 ^ 5))
            , Just (T.Tile (3 * 2 ^ 6))
            , Just (T.Tile (3 * 2 ^ 7))
            , Just (T.Tile (3 * 2 ^ 8))
            ]
        , V.Vector
            [ Just (T.Tile (3 * 2 ^ 9))
            , Just (T.Tile (3 * 2 ^ 10))
            , Just (T.Tile (3 * 2 ^ 11))
            , Nothing
            ]
        ]
