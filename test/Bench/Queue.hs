module Bench.Queue (
    runQueueBench,
) where

import Criterion
import Criterion.Main

import Queue.Class
import qualified Queue.Persistent as Persistent
import qualified Queue.RealTime as RealTime
import qualified Queue.Transient as Transient
import Queue.Utils


-- | Run all queue benchmarks

runQueueBench :: IO ()
runQueueBench = defaultMain [
  runOneBench 1000  ,
  runOneBench 10000 ]

runOneBench :: Int -> Benchmark
runOneBench n =
  let name = "Queue_" ++ show n
  in bgroup name [
      bgroup "NoPersist" [
          bench "Transient"  $ nf (testNoPersist n)  Transient.create  ,
          bench "RealTime"   $ nf (testNoPersist n)  RealTime.create   ,
          bench "Persistent" $ nf (testNoPersist n)  Persistent.create ]
      ,
      bgroup "WithPersist" [
          bench "Transient"  $ nf (testWithPersist n)  Transient.create  ,
          bench "RealTime"   $ nf (testWithPersist n)  RealTime.create   ,
          bench "Persistent" $ nf (testWithPersist n)  Persistent.create ]
      ]


-- ^ Operation count: 3 * n
testNoPersist :: (IQueue q) => Int -> ([Int] -> q Int) -> Int
testNoPersist n create =
    let q1 = create [1 :: Int]
        q2 = foldl (flip push) q1 [1 .. n]
        q3 = iterate (pop . push 1) q2 !! n
    in top q3


-- ^ Operation count: 201 * n
testWithPersist :: (IQueue q) => Int -> ([Int] -> q Int) -> Int
testWithPersist n create =
    let q1 = create [1 :: Int]
        q2 = scanl (flip push) q1 [1 .. n]
        qs = fmap ((!! 100) . iterate (pop . push 1)) q2
    in sum $ fmap top qs
