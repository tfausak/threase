module Main (main) where

import           Criterion.Main         (bgroup, defaultMain)
import qualified Threase.BoardBench
import qualified Threase.DirectionBench
import qualified Threase.GameBench
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
    , bgroup "Threase.Game" Threase.GameBench.benchmarks
    , bgroup "Threase.Direction" Threase.DirectionBench.benchmarks
    -- , bgroup "X.X.X" X.X.XBench.benchmarks
    ]
