module Threase.VectorBench (benchmarks) where

import Criterion
import Threase.Vector

benchmarks :: [Benchmark]
benchmarks =
    [ bench "squish" $ whnf squish (Vector [])
    ]
