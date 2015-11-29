module Bench.Queue (
    runQueueBench,
) where


import Criterion
import Criterion.Main

import Queue.Class
import qualified Queue.Persistent as Persistent
import qualified Queue.Transient as Transient
import Queue.Utils

import System.IO


runQueueBench :: IO ()
runQueueBench = do
    -- Settings for better output by criterion
    hSetBuffering stdout NoBuffering
    hSetEncoding stdout utf8

    -- Test cases for criterion
    defaultMain [
        bgroup "Queue" [
            bgroup "NoPersist" [
                bench "Transient"  $ nf (testNoPersist 1000)  Transient.create  ,
                bench "Persistent" $ nf (testNoPersist 1000)  Persistent.create ,
                bench "Transient"  $ nf (testNoPersist 10000) Transient.create  ,
                bench "Persistent" $ nf (testNoPersist 10000) Persistent.create ]
            ,
            bgroup "WithPersist" [
                bench "Transient"  $ nf (testWithPersist 1000)  Transient.create  ,
                bench "Persistent" $ nf (testWithPersist 1000)  Persistent.create ,
                bench "Transient"  $ nf (testWithPersist 10000) Transient.create  ,
                bench "Persistent" $ nf (testWithPersist 10000) Persistent.create ]
            ]
        ]


testNoPersist :: (IQueue q) => Int -> ([Int] -> q Int) -> Int
testNoPersist n create =
    let q1 = create [0..100 :: Int]
        q2 = foldl (flip push) q1 [100 .. 100 + n]
        q3 = iterate (pop . push 1) q2 !! 1000
    in top q3


testWithPersist :: (IQueue q) => Int -> ([Int] -> q Int) -> Int
testWithPersist n create =
    let q1 = create [1 :: Int]
        q2 = foldl (flip push) q1 [1 .. n]
        qs = replicate 1000 (pop q2)
    in sum $ fmap top qs

