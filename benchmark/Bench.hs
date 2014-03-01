module Main (main) where

import           Criterion.Main      (bgroup, defaultMain)
import qualified Threase.BoardBench
import qualified Threase.TileBench
import qualified Threase.VectorBench
import qualified ThreaseBench
-- import qualified X.X.XBench

main :: IO ()
main = defaultMain
    [ bgroup "Threase" ThreaseBench.benchmarks
    , bgroup "Threase.Tile" Threase.TileBench.benchmarks
    , bgroup "Threase.Vector" Threase.VectorBench.benchmarks
    , bgroup "Threase.Board" Threase.BoardBench.benchmarks
    -- , bgroup "X.X.X" X.X.XBench.benchmarks
    ]
