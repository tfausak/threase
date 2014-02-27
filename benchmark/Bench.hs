module Main (main) where

import           Criterion.Main (bgroup, defaultMain)
import qualified ThreaseBench

main :: IO ()
main = defaultMain
    [ bgroup "Threase" ThreaseBench.benchmarks
    ]
