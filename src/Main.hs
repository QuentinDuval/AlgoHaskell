module Main (
    main
) where

import Benchs
import RMQ.RangeQuery


main :: IO ()
main = do
    let q = rangeFromList (fmap Min [1 .. 8])
    print q
    print $ rangeQuery q (0,0)
    print $ elementAt  q 0
    print $ elementAt  q 1
    print $ rangeQuery q (0,8)
    print $ rangeQuery q (1,8)
    print $ rangeQuery q (2,5)
    print $ rangeQuery q (7,8)

--    runAllBenchs
    return ()

