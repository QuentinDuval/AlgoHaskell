module Main (
    main
) where

import Benchs
import RMQ.RangeQuery


main :: IO ()
main = do
    let q1 = rangeFromList (fmap Min [1 .. 8])
    testQueue q1
    let q2 = updateVal (updateVal q1 0 (Min 8)) 2 (Min 8)
    testQueue q2

--    runAllBenchs
    return ()

testQueue :: RMQ Min -> IO ()
testQueue q = do
    print q
    print $ rangeQuery q (0,0)
    print $ elementAt  q 0
    print $ elementAt  q 1
    print $ rangeQuery q (0,8)
    print $ rangeQuery q (1,8)
    print $ rangeQuery q (2,5)
    print $ rangeQuery q (7,8)

