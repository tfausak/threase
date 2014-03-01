module Threase.VectorBench (benchmarks) where

import           Criterion
import           Threase.Vector

benchmarks :: [Benchmark]
benchmarks =
    [ bench "canShift" $ whnf canShift (Vector [])
    , bench "render" $ whnf render (Vector [])
    , bench "score" $ whnf score (Vector [])
    , bench "shift" $ whnf shift (Vector [])
    ]
