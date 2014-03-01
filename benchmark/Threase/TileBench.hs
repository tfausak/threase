module Threase.TileBench (benchmarks) where

import           Criterion
import           Threase.Tile

benchmarks :: [Benchmark]
benchmarks =
    [ bench "add" $ whnf (add (Tile 3)) (Tile 3)
    , bench "canAdd" $ whnf (canAdd (Tile 3)) (Tile 3)
    , bench "render" $ whnf render (Tile 3)
    , bench "score" $ whnf score (Tile 3)
    ]
