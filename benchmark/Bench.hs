module Main (main) where

import           Criterion.Main         (bgroup, defaultMain)
import qualified Threase.BoardBench
import qualified Threase.DirectionBench
import qualified Threase.GameBench
import qualified Threase.TileBench
import qualified Threase.VectorBench
import qualified ThreaseBench
-- HASKELETON: import qualified New.Module

main :: IO ()
main = defaultMain
    [ bgroup "Threase" ThreaseBench.benchmarks
    , bgroup "Threase.Board" Threase.BoardBench.benchmarks
    , bgroup "Threase.Direction" Threase.DirectionBench.benchmarks
    , bgroup "Threase.Game" Threase.GameBench.benchmarks
    , bgroup "Threase.Tile" Threase.TileBench.benchmarks
    , bgroup "Threase.Vector" Threase.VectorBench.benchmarks
    -- HASKELETON: , bgroup "New.Module" New.ModuleBench.benchmarks
    ]
