module Main (main) where

import Criterion.Main
import qualified ThreaseBench

main :: IO ()
main = defaultMain
    [ bgroup "Threase" ThreaseBench.benchmarks
    ]
