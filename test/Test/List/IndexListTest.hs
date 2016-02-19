module Test.List.IndexListTest (
  runIndexListTests,
) where

import Data.Foldable
import qualified List.IndexList as Naive
import qualified List.SkewIndexList as Skew
import List.IndexListClass hiding (empty, fromList)
import Test.HUnit


-- | All tests

runIndexListTests :: Test
runIndexListTests = TestList [
    TestLabel "Naive" $
      TestList $ [ pushPopTest, foldableTest, indexAccessTest, indexUpdateTest ] <*> [ Naive.fromList ],
    TestLabel "Skew" $
      TestList $ [ pushPopTest, foldableTest, indexAccessTest, indexUpdateTest ] <*> [ Skew.fromList ]
  ]

type Constructor l a = ([a] -> l Int)


-- | Test cases

pushPopTest :: (IIndexList l) => Constructor l Int -> Test
pushPopTest create =
  let l1 = create [0 .. 100]
      l2 = getTail $ getTail l1
  in TestCase $ do
    assertEqual "Old Head" 0 $ getHead l1
    assertEqual "New Head" 2 $ getHead l2

foldableTest :: (IIndexList l) => Constructor l Int -> Test
foldableTest create =
  let l = create [0 .. 100]
  in TestCase $ do
    assertEqual "Length"  101      $ length l
    assertEqual "To List" [0..100] $ toList l
    assertEqual "Functor" [1..101] $ toList (fmap (+1) l)
    assertEqual "Sum all" 5050     $ sum l

indexAccessTest :: (IIndexList l) => Constructor l Int -> Test
indexAccessTest create =
  let l = create [0 .. 100]
  in TestCase $ do
      assertEqual "Element at" 0   $ l `at` 0
      assertEqual "Element at" 50  $ l `at` 50
      assertEqual "Element at" 100 $ l `at` 100

indexUpdateTest :: (IIndexList l) => Constructor l Int -> Test
indexUpdateTest create =
  let temp = create [0 .. 100]
      l = foldl (updateAt (+ 1)) temp [0, 50, 100]
  in TestCase $ do
      assertEqual "Update at" 1   $ l `at` 0
      assertEqual "Update at" 51  $ l `at` 50
      assertEqual "Update at" 101 $ l `at` 100
