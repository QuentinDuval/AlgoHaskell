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
            bench "Transient"  $ nf testQueue Transient.create  ,
            bench "Persistent" $ nf testQueue Persistent.create ]
        ]


testQueue :: (IQueue q) => ([Int] -> q Int) -> [Int]
testQueue create =
    let q1 = push 12 $ push 11 $ pop $ create [0..10 :: Int]
        q2 = foldl (flip push) q1 [13 .. 100 :: Int]
    in consume (:) [] q2

