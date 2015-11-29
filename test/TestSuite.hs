module TestSuite (
    runTestSuite,
) where

import Test.HUnit


runTestSuite :: IO Counts
runTestSuite = runTestTT $ TestList []

