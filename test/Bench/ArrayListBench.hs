module Bench.ArrayListBench (
  runArrayListBench
) where

import Criterion
import Criterion.Main

import Control.Monad.ST
import Data.List
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Vector.Algorithms.Intro as V
import qualified Data.Vector.Persistent as PV


runArrayListBench :: IO ()
runArrayListBench = defaultMain [
    bgroup "ArrayList" [
      runSumBench 1000000,
      runSortBench 1000000
    ]
  ]

--------------------------------------------------------------------------------

runSumBench :: Int -> Benchmark
runSumBench n = bgroup ("SumBench" ++ show n) [
    bench "PersistentVector"  $ nf sumPersistentVector n,
    bench "StandardList"      $ nf sumStandardList n,
    bench "StandardVector"    $ nf sumStandardVector n
  ]

sumPersistentVector :: Int -> Int
sumPersistentVector n = PV.foldl' (+) 0 $ iterate (`PV.snoc` 1) PV.empty !! n

sumStandardList :: Int -> Int
sumStandardList n = sum $ iterate (1 :) [] !! n

sumStandardVector :: Int -> Int
sumStandardVector n = V.foldl' (+) 0 $ V.constructN n (const 1)

--------------------------------------------------------------------------------

runSortBench :: Int -> Benchmark
runSortBench n =
  let l = take n (cycle [1..div n 100])
  in bgroup ("SortBench" ++ show n) [
    bench "StandardList"      $ nf sortStandardList l,
    bench "StandardVector"    $ nf sortStandardVector l
  ]

sortStandardVector :: [Int] -> V.Vector Int
sortStandardVector l =
  runST $ do
    v <- V.unsafeThaw (V.fromList l)
    V.sort v
    V.unsafeFreeze v

sortStandardList :: [Int] -> [Int]
sortStandardList = sort

--------------------------------------------------------------------------------
