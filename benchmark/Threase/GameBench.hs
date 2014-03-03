module Threase.GameBench (benchmarks) where

import           Criterion
import           Threase.Board  (Board (..))
import           Threase.Game
import           Threase.Tile   (Tile (..))
import           Threase.Vector (Vector (..))

benchmarks :: [Benchmark]
benchmarks =
    [ bench "quality" $ whnf quality game
    ]
  where
    game = Game $ Board
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
