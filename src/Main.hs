module Main (
    main
) where

import Benchs
import System.Environment


main :: IO ()
main = do
  args <- getArgs
  runAllBenchs args
