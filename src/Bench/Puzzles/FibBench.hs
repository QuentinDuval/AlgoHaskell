module Bench.Puzzles.FibBench (
    runFibBench,
) where


import Criterion
import Criterion.Main
import Puzzles.Fibonacci



runFibBench :: IO ()
runFibBench =
    defaultMain [
        bgroup "Fibonacci" [
            bench "fibIterate"  $ nf fibIterate 4000,
            bench "fibMatrix"   $ nf fibMatrix  4000
        ]
    ]

