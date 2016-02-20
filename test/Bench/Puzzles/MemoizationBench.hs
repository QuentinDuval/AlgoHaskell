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
            cutRodBench,
            memoSparseBench
        ]
      ]


cutRodBench :: Benchmark
cutRodBench =
  let inputs = V.fromList [1 .. 1000]
  in bgroup "CutRod" [
        bench "memoCutRod"      $ nf memoCutRod     inputs,
        bench "memoCutRod2"     $ nf memoCutRod2    inputs
     ]

memoSparseBench :: Benchmark
memoSparseBench =
 let inputs = 1000000
 in bgroup "MemoSparse" [
       bench "memoList"      $ nf memoList     inputs,
       bench "memoVector"    $ nf memoVector   inputs,
       bench "memoTree"      $ nf memoTree     inputs,
       bench "memoLazyMap"   $ nf memoMap      inputs,
       bench "memoModule"    $ nf memoModule   inputs,
       bench "memoMonadMap"  $ nf memoMonad    inputs
    ]
