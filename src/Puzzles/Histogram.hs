module Puzzles.Histogram where

import Control.Arrow
import Data.List
import qualified Data.Map as M
import Utils.FormatString


{-
Exercise taken from the course CIS-194:
http://www.seas.upenn.edu/~cis194/spring13/hw/03-rec-poly.pdf
-}

-- | Displaying an histogram

horizontalBars :: [Int] -> [String]
horizontalBars vals =
  let totalLen = maximum vals
      horBar n = replicate n '*' ++ repeat ' '
  in fmap (take totalLen . horBar) vals

toVerticalBars :: [String] -> [String]
toVerticalBars = fmap reverse >>> transpose

horizontalLegends :: (Int, Int) -> [String]
horizontalLegends (start, end) = padL lenSize <$> legends
  where
    legends = show <$> [start .. end]
    lenSize = maximum (fmap length legends)

verticalLegends :: (Int, Int) -> [String]
verticalLegends r@(start, end) = sepLine : transpose (horizontalLegends r)
  where sepLine = replicate (end - start + 1) '='

showHistogram :: (Int, Int) -> [Int] -> String
showHistogram range =
  horizontalBars
    >>> toVerticalBars
    >>> (++ verticalLegends range)
    >>> intercalate "\n"


-- | Counting the elements in the list

frequencies :: (Foldable f, Ord a) => Int -> f a -> M.Map a Int
frequencies increment = foldl (\m k -> M.insertWith (+) k increment m) M.empty

frequenciesIn :: (Int, Int) -> [Int] -> [Int]
frequenciesIn (start, end) =
  let range = [start..end]
  in filter (`elem` range)                      -- Remove elements out of the range
      >>> frequencies 1                         -- Count the number of each element
      >>> M.unionWith (+) (frequencies 0 range) -- Add missing elements with count 0
      >>> fmap snd . M.toAscList                -- Return the counts


-- | Assembling the pieces together

histogram :: (Int, Int) -> [Int] -> String
histogram r = frequenciesIn r >>> showHistogram r


-- Test function

test :: IO ()
test = do
    let input = [11,1,4,5,4,6,6,3,4,2,4,9,11,-1]
    putStrLn ""
    putStrLn $ histogram (0,9) input
    putStrLn ""
    putStrLn $ histogram (-1,11) input
