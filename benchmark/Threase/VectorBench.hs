module Threase.VectorBench (benchmarks) where

import           Criterion
import           Threase.Vector

benchmarks :: [Benchmark]
benchmarks =
    [ bench "shift" $ whnf shift (Vector [])
    ]
