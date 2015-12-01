module TestSuite (
    runTestSuite,
) where

import Test.HUnit
import Test.Queue
import Test.RMQ


runTestSuite :: IO Counts
runTestSuite = runTestTT $ TestList [ runQueueTests, runRangeQueryTests ]

