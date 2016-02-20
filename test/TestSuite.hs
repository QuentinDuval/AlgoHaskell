module TestSuite (
    runTestSuite,
) where

import Test.HUnit

import Test.Graph.BfsTest
import Test.Graph.DfsTest
import Test.List.IndexListTest
import Test.Queue
import Test.Puzzles.FibTest
import Test.Puzzles.MemoizationTest
import Test.RMQ
import Test.RoseTreeTest


runTestSuite :: IO Counts
runTestSuite = runTestTT $ TestList
  [ runQueueTests, runRangeQueryTests, runFibTests, runAllMemoTests
  , runDfsTests, runBfsTests, runRoseTreeTests, runIndexListTests ]
