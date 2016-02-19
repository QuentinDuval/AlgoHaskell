module Test.List.IndexListTest (
  runIndexListTests,
) where

import Data.Foldable
import qualified List.IndexList as Naive
import qualified List.SkewIndexList as Skew
import List.IndexListClass
import Test.HUnit


-- | All tests

runIndexListTests :: Test
runIndexListTests = TestList
    [ pushPopTest, foldableTest, indexAccessTest, indexUpdateTest ]


-- | Test cases

pushPopTest :: Test
pushPopTest =
  let l1 = fromList [0 .. 100] :: Naive.IndexedList Int
      l2 = getTail $ getTail l1
  in TestCase $ do
    assertEqual "Old Head" 0 $ getHead l1
    assertEqual "New Head" 2 $ getHead l2


foldableTest :: Test
foldableTest =
  let l = fromList [0 .. 100] :: Naive.IndexedList Int
  in TestCase $ do
    assertEqual "Length"  101      $ length l
    assertEqual "To List" [0..100] $ toList l
    assertEqual "Functor" [1..101] $ toList (fmap (+1) l)
    assertEqual "Sum all" 5050     $ sum l

indexAccessTest :: Test
indexAccessTest =
  let l = fromList [0 .. 100] :: Naive.IndexedList Int
  in TestCase $ do
      assertEqual "Element at" 0   $ l `at` 0
      assertEqual "Element at" 50  $ l `at` 50
      assertEqual "Element at" 100 $ l `at` 100

indexUpdateTest :: Test
indexUpdateTest =
  let temp = fromList [0 .. 100] :: Naive.IndexedList Int
      l = foldl (updateAt (+ 1)) temp [0, 50, 100]
  in TestCase $ do
      assertEqual "Element at" 1   $ l `at` 0
      assertEqual "Element at" 51  $ l `at` 50
      assertEqual "Element at" 101 $ l `at` 100
