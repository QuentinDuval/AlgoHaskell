module Test.Graph.BfsTest (
    runBfsTests,
) where

import Graph
import Test.HUnit


-- | All tests

runBfsTests :: Test
runBfsTests = TestList [dagBfsTest, cyclicBfsTest, bfsLazinessTest]


-- | Test cases

dagBfsTest :: Test
dagBfsTest =
    let g = makeAdjList [(1, [2, 3]),
                         (2, [3, 4]),
                         (3, [5]),
                         (4, [5, 6])]
    in TestCase $ do
        assertEqual "Bfs on a dag 1" [1, 2, 3, 4, 5, 6] (bfsFrom g 1)
        assertEqual "Bfs on a dag 2" [3, 5]             (bfsFrom g 3)


cyclicBfsTest :: Test
cyclicBfsTest =
    let g = makeAdjList [(i, [mod i 10 + 1]) | i <- [1..10]]
    in TestCase $ do
        assertEqual "Cyclic graph 1" [1..10]             (bfsFrom g 1)
        assertEqual "Cyclic graph 2" ([5..10] ++ [1..4]) (bfsFrom g 5)


bfsLazinessTest :: Test
bfsLazinessTest =
    let n = 100000
        g = makeAdjList [(i, [mod i n + 1]) | i <- [1..n]]
    in TestCase $ do
        assertEqual "Stop at target" [1..9] (takeWhile (/= 10) $ bfsFrom g 1)
        assertEqual "Stop at number" [1..9] (take 9            $ bfsFrom g 1)

