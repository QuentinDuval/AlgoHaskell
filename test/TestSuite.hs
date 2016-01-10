module TestSuite (
    runTestSuite,
) where

import Test.HUnit

import Test.Graph.BfsTest
import Test.Graph.DfsTest
import Test.Queue
import Test.Puzzles.FibTest
import Test.RMQ
import Test.RoseTreeTest


runTestSuite :: IO Counts
runTestSuite = runTestTT $ TestList
  [ runQueueTests, runRangeQueryTests, runFibTests
  , runDfsTests, runBfsTests, runRoseTreeTests ]
