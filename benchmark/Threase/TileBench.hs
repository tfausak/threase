module Threase.TileBench (benchmarks) where

import           Criterion
import           Threase.Tile

benchmarks :: [Benchmark]
benchmarks =
    [ bench "add" $ whnf (add tile) tile
    , bench "canAdd" $ whnf (canAdd tile) tile
    , bench "render" $ whnf render tile
    , bench "score" $ whnf score tile
    ]
  where
    tile = Tile 3
