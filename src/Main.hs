module Main (
    main
) where

import Criterion
import Criterion.Main

import Queue.Class
import qualified Queue.Persistent as Persistent
import qualified Queue.Transient as Transient
import Queue.Utils

import System.IO


main :: IO ()
main = do
    -- Settings for better output by criterion
    hSetBuffering stdout NoBuffering
    hSetEncoding stdout utf8

    -- Test cases for criterion
    defaultMain [
        bgroup "Queue" [
            bgroup "NoPersist" [
                bench "Transient"  $ nf testNoPersist   Transient.create,
                bench "Persistent" $ nf testNoPersist   Persistent.create ]
            ,
            bgroup "WithPersist" [
                bench "Transient"  $ nf testWithPersist Transient.create,
                bench "Persistent" $ nf testWithPersist Persistent.create ]
            ]
        ]


testNoPersist :: (IQueue q) => ([Int] -> q Int) -> Int
testNoPersist create =
    let q1 = create [0..10 :: Int]
        q2 = foldl (flip push) q1 [100 .. 1000]
        q3 = iterate (pop . push 1) q2 !! 1000
    in top q3


testWithPersist :: (IQueue q) => ([Int] -> q Int) -> Int
testWithPersist create =
    let q1 = create [1 :: Int]
        q2 = foldl (flip push) q1 [100 .. 1000]
        qs = replicate 100000 (pop q2)
    in sum $ fmap top qs

