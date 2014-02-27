module Main (main) where

import           Criterion.Main    (bgroup, defaultMain)
import qualified Threase.TileBench
import qualified ThreaseBench
-- import qualified X.X.XBench

main :: IO ()
main = defaultMain
    [ bgroup "Threase" ThreaseBench.benchmarks
    , bgroup "Threase.Tile" Threase.TileBench.benchmarks
    -- , bgroup "X.X.X" X.X.XBench.benchmarks
    ]
