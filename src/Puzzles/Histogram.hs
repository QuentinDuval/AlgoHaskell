module Puzzles.Histogram where

import Control.Arrow
import Data.List
import qualified Data.Map as M
import Utils.FormatString


-- | Displaying an histogram

horizontalBars :: [Int] -> [String]
horizontalBars vals =
  let len = maximum vals
      bar n = replicate n '*' ++ repeat ' '
  in fmap (take len . bar) vals

toVerticalBars :: [String] -> [String]
toVerticalBars = fmap reverse >>> transpose

horizontalLegends :: (Int, Int) -> [String]
horizontalLegends (start, end) = padL lenSize <$> legends
  where
    legends = show <$> [start..end]
    lenSize = maximum (length <$> legends)

verticalLegends :: (Int, Int) -> [String]
verticalLegends r@(start, end) = sepLine : transpose (horizontalLegends r)
  where sepLine = replicate (end - start + 1) '='

showHistogram :: (Int, Int) -> [Int] -> String
showHistogram r =
  horizontalBars
    >>> toVerticalBars
    >>> (++ verticalLegends r)
    >>> intercalate "\n"


-- | Counting the elements in the list

frequencies :: (Foldable f, Ord a) => Int -> f a -> M.Map a Int
frequencies increment = foldl (\m k -> M.insertWith (+) k increment m) M.empty

frequenciesIn :: (Int, Int) -> [Int] -> [Int]
frequenciesIn (start, end) =
  let r = [start..end]
  in filter (`elem` r)                        -- Remove elements out of the range
      >>> frequencies 1                       -- Count the number of each element
      >>> M.unionWith (+) (frequencies 0 r)   -- Add missing elements with count 0
      >>> fmap snd . M.toAscList              -- Return the counts


-- | Assembling the pieces together

histogram :: (Int, Int) -> [Int] -> String
histogram r = frequenciesIn r >>> showHistogram r


-- Test function

test :: IO ()
test = do
    let input = [11,1,4,5,4,6,6,3,4,2,4,9,11,-1]
    putStrLn $ histogram (0,9) input
    putStrLn $ histogram (-1,11) input
