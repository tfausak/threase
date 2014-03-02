module Threase.VectorBench (benchmarks) where

import           Criterion
import qualified Threase.Tile   as T
import           Threase.Vector

benchmarks :: [Benchmark]
benchmarks =
    [ bench "canShift" $ whnf canShift vector
    , bench "render" $ whnf render vector
    , bench "score" $ whnf score vector
    , bench "shift" $ whnf shift vector
    ]
  where
    vector = Vector
        [ Just (T.Tile 1)
        , Just (T.Tile 2)
        , Nothing
        , Just (T.Tile 3)
        ]
