module Test.List.IndexListTest (
  runIndexListTests,
) where


import List.IndexList
import Test.HUnit

-- | All tests

runIndexListTests :: Test
runIndexListTests = TestList
    [ indexAccessTest, indexUpdateTest ]


-- | Test cases

indexAccessTest :: Test
indexAccessTest =
  let l = fromList [0 .. 100 :: Int]
  in TestCase $ do
      assertEqual "Element at" 0   $ l `at` 0
      assertEqual "Element at" 50  $ l `at` 50
      assertEqual "Element at" 100 $ l `at` 100

indexUpdateTest :: Test
indexUpdateTest =
  let temp = fromList [0 .. 100 :: Int]
      l = foldl (updateAt (+ 1)) temp [0, 50, 100]
  in TestCase $ do
      assertEqual "Element at" 1   $ l `at` 0
      assertEqual "Element at" 51  $ l `at` 50
      assertEqual "Element at" 101 $ l `at` 100
