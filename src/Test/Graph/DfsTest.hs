module Test.Graph.DfsTest (
    runDfsTests,
) where


import Graph
import Test.HUnit



-- | All tests

runDfsTests :: Test
runDfsTests = TestList [dagDfsTest, cyclicDfsTest, dfsLazinessTest]


-- | Test cases

dagDfsTest :: Test
dagDfsTest =
    let g = makeAdjList [(1, [2, 3]),
                         (2, [3, 4]),
                         (3, [5]),
                         (4, [5, 6])]
    in TestCase $ do
        assertEqual "Dfs on a dag 1" [1, 2, 3, 5, 4, 6] (dfsFrom g 1)
        assertEqual "Dfs on a dag 2" [3, 5]             (dfsFrom g 3)


cyclicDfsTest :: Test
cyclicDfsTest =
    let g = makeAdjList [(i, [mod i 10 + 1]) | i <- [1..10]]
    in TestCase $ do
        assertEqual "Cyclic graph 1" [1..10]             (dfsFrom g 1)
        assertEqual "Cyclic graph 2" ([5..10] ++ [1..4]) (dfsFrom g 5)


dfsLazinessTest :: Test
dfsLazinessTest =
    let n = 100000
        g = makeAdjList [(i, [mod i n + 1]) | i <- [1..n]]
    in TestCase $ do
        assertEqual "Stop at target" [1..9] (takeWhile (/= 10) $ dfsFrom g 1)
        assertEqual "Stop at number" [1..9] (take 9            $ dfsFrom g 1)

