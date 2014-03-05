module Threase.VectorBench (benchmarks) where

import           Criterion
import           Threase.Tile   (Tile (..))
import           Threase.Vector

benchmarks :: [Benchmark]
benchmarks =
    [ bench "canShift" $ whnf canShift vector
    , bench "render" $ whnf render vector
    , bench "score" $ whnf score vector
    , bench "shift" $ whnf shift vector
    , bench "shiftWith" $ whnf shift vector
    ]
  where
    vector = Vector
        [ Just (Tile 1)
        , Just (Tile 2)
        , Nothing
        , Just (Tile 3)
        ]
