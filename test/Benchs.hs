module Benchs (
    runAllBenchs
) where


import Bench.Queue
import Bench.Puzzles.FibBench
import Bench.Puzzles.MemoizationBench
import qualified Data.Set as S
import System.IO


-- | Run all benches provided as arguments

runAllBenchs :: [String] -> IO ()
runAllBenchs args = do
  -- Settings for better output by criterion
  hSetBuffering stdout NoBuffering
  hSetEncoding stdout utf8
  -- Run all unique benches
  let noDup = S.toList $ S.fromList args
  mapM_ runBench noDup


-- | Available benches

runBench :: String -> IO ()
runBench "Queue"      = runQueueBench
runBench "Fibonacci"  = runFibBench
runBench "Memoize"    = runMemoBench
runBench _            = pure ()
