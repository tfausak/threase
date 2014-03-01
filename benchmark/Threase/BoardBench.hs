module Threase.BoardBench (benchmarks) where

import           Criterion
import           Threase.Board

benchmarks :: [Benchmark]
benchmarks =
    [ bench "score" $ whnf score (Board [])
    ]
