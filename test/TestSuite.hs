module TestSuite (
    runTestSuite,
) where

import Test.HUnit
import Test.Queue

runTestSuite :: IO Counts
runTestSuite = runTestTT $ TestList [ runQueueTests ]

