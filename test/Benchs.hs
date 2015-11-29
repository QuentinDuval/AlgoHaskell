module Benchs (
    runAllBenchs
) where


import Bench.Queue

runAllBenchs :: IO ()
runAllBenchs = do
    runQueueBench
