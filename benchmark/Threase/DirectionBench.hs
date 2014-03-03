module Threase.DirectionBench (benchmarks) where

import           Criterion
import           Threase.Direction

benchmarks :: [Benchmark]
benchmarks =
    [ bench "render" $ whnf render West
    ]
