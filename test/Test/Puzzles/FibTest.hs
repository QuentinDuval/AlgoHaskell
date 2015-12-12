module Test.Puzzles.FibTest (
    runFibTests,
) where


import Puzzles.Fibonacci
import Test.HUnit


-- | All tests

runFibTests :: Test
runFibTests = TestList [
        runFibTest "fibIterate" fibIterate,
        runFibTest "fibImperative" fibImperative,
        runFibTest "fibMatrix" fibMatrix,
        runFibTest "fibMatrixTR" fibMatrixTR
    ]


runFibTest :: String -> (Int -> Integer) -> Test
runFibTest str fibImpl =
    TestCase $ assertEqual str [0, 1, 1, 2, 3, 5, 8, 13, 21, 34] (fmap fibImpl [0 .. 9])

