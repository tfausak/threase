module Threase.BoardBench (benchmarks) where

import           Criterion
import           Threase.Board

benchmarks :: [Benchmark]
benchmarks =
    [ bench "canShift" $ whnf canShift (Board [])
    , bench "score" $ whnf score (Board [])
    , bench "shift" $ whnf shift (Board [])
    ]
