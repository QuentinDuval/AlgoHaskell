module Test.Puzzles.MemoizationTest (
    runAllMemoTests,
) where

import qualified Data.Vector as V
import Puzzles.Memoization
import Test.HUnit


-- | All tests

runAllMemoTests :: Test
runAllMemoTests = TestList [
        runMemoTest "memoCutRod"  memoCutRod,
        runMemoTest "memoCutRod2" memoCutRod2
    ]


runMemoTest :: String -> (V.Vector Int -> Int) -> Test
runMemoTest str cutRodImpl =
    TestCase $ do
      assertEqual str 100 (cutRodImpl $ V.fromList [1..100])
      assertEqual str 100 (cutRodImpl $ V.fromList $ replicate 100 1)
      assertEqual str 10  (cutRodImpl $ V.fromList [1, 5, 8, 9])
