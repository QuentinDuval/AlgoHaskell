module Bench.Puzzles.MemoizationBench (
  runMemoBench,
) where


import Criterion
import Criterion.Main
import Puzzles.Memoization
import qualified Data.Vector as V


runMemoBench :: IO ()
runMemoBench =
  let inputs = V.fromList [1 .. 1000]
  in defaultMain [
        bgroup "Memoize" [
            bench "memoCutRod"      $ nf memoCutRod     inputs,
            bench "memoCutRod2"     $ nf memoCutRod2    inputs
        ]
      ]
