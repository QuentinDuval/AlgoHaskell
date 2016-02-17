module Benchs (
    runAllBenchs
) where


import Bench.Queue
import Bench.Puzzles.FibBench
import System.IO


runAllBenchs :: IO ()
runAllBenchs = do
    -- Settings for better output by criterion
    hSetBuffering stdout NoBuffering
    hSetEncoding stdout utf8

    -- Run all benches
    runQueueBench
    -- runFibBench
