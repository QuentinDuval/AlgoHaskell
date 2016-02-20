module Test.Puzzles.MemoizationTest (
    runAllMemoTests,
) where

import qualified Data.Vector as V
import Puzzles.Memoization
import Test.HUnit


-- | All tests

runAllMemoTests :: Test
runAllMemoTests = TestList [
        runCutRodTest   "memoCutRod"  memoCutRod,
        runCutRodTest   "memoCutRod2" memoCutRod2,
        runKnapsackTest "naiveKnapsack" naiveKnapsack,
        runKnapsackTest "memoKnapsack"  memoKnapsack
    ]


runCutRodTest :: String -> (V.Vector Int -> Int) -> Test
runCutRodTest str cutRodImpl =
    TestCase $ do
      assertEqual str 100 (cutRodImpl $ V.fromList [1..100])
      assertEqual str 100 (cutRodImpl $ V.fromList $ replicate 100 1)
      assertEqual str 10  (cutRodImpl $ V.fromList [1, 5, 8, 9])


runKnapsackTest :: String -> (V.Vector Item -> Int -> Int) -> Test
runKnapsackTest str algoImpl =
  let items = V.fromList [(1, 1), (2, 3), (5, 5)]
  in TestCase $ do
    assertEqual str 0 (algoImpl items 0)
    assertEqual str 1 (algoImpl items 1)
    assertEqual str 3 (algoImpl items 2)
    assertEqual str 4 (algoImpl items 3)
    assertEqual str 5 (algoImpl items 5)
    assertEqual str 6 (algoImpl items 6)
    assertEqual str 8 (algoImpl items 7)
    assertEqual str 9 (algoImpl items 8)
