module Bench.Queue (
    runQueueBench,
) where

import Criterion
import Criterion.Main

import Queue.Class
import qualified Queue.DataSeq as DataSeq
import qualified Queue.Persistent as Persistent
import qualified Queue.RealTime as RealTime
import qualified Queue.Stack as Stack
import qualified Queue.Transient as Transient
import Queue.Utils


-- | Run all queue benchmarks

runQueueBench :: IO ()
runQueueBench = defaultMain [
    bgroup "Queue" [
      runNoPersist   1000, runNoPersist   5000 ,
      runWithPersist 1000, runWithPersist 5000 ]
  ]

runNoPersist :: Int -> Benchmark
runNoPersist n = bgroup ("NoPersist_" ++ show n) [
    bench "Transient"  $ nf (testNoPersist n)  Transient.create  ,
    bench "RealTime"   $ nf (testNoPersist n)  RealTime.create   ,
    bench "Persistent" $ nf (testNoPersist n)  Persistent.create ,
    bench "DataSeq"    $ nf (testNoPersist n)  DataSeq.create    ,
    bench "Stack"      $ nf (testNoPersist n)  Stack.create      ]

runWithPersist :: Int -> Benchmark
runWithPersist n = bgroup ("WithPersist_" ++ show n) [
    bench "Transient"  $ nf (testWithPersist n)  Transient.create  ,
    bench "RealTime"   $ nf (testWithPersist n)  RealTime.create   ,
    bench "Persistent" $ nf (testWithPersist n)  Persistent.create ,
    bench "DataSeq"    $ nf (testWithPersist n)  DataSeq.create    ,
    bench "Stack"      $ nf (testWithPersist n)  Stack.create      ]


-- ^ Operation count: 4 * n
testNoPersist :: (IQueue q) => Int -> ([Int] -> q Int) -> Int
testNoPersist n create =
    let q1 = create [1 :: Int]
        q2 = foldl (flip push) q1 [1 .. n]
        q3 = iterate (pop . push 1) q2 !! n
    in consume (+) 0 q3


-- ^ Operation count: 22 * n
testWithPersist :: (IQueue q) => Int -> ([Int] -> q Int) -> Int
testWithPersist n create =
    let q1 = create [1 :: Int]
        q2 = scanl (flip push) q1 [1 .. n]
        qs = fmap ((!! 10) . iterate (pop . push 1)) q2
    in sum $ fmap top qs
