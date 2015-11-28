module Main (
    main
) where

import Queue.Class
import qualified Queue.Persistent as Persistent
import qualified Queue.Transient as Transient
import Queue.Utils


main :: IO ()
main = do
    testQueue Transient.create
    testQueue Persistent.create



testQueue :: (IQueue q) => ([Int] -> q Int) -> IO ()
testQueue create = do
    let q1 = push 12 $ push 11 $ pop $ create [0..10 :: Int]
    let q2 = foldl (flip push) q1 [13 .. 100 :: Int]
    print $ consume (:) [] q2
