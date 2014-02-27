module Main (main) where

import           Criterion.Main (bgroup, defaultMain)
import qualified ThreaseBench
-- import qualified X.X.XBench

main :: IO ()
main = defaultMain
    [ bgroup "Threase" ThreaseBench.benchmarks
    -- , bgroup "X.X.X" X.X.XBench.benchmarks
    ]
