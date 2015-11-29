module Main (
    main
) where


import Control.Monad
import System.Exit (exitFailure)
import Test.HUnit
import TestSuite


main :: IO ()
main = do
    r <- runTestSuite
    when (failures r > 0) exitFailure


