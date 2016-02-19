module Puzzles.Calendar (

) where

import Control.Arrow
import Data.Function
import Data.List
import Data.List.Split
import Utils.Time
import Utils.FormatString


-- | Building a calendar
-- Inspired by the presentation of Eric Niebler at CppCon 2015:
-- https://www.youtube.com/watch?v=mFUXNMfaciE

groupByMonth :: [Day] -> [[Day]]
groupByMonth = groupBy ((==) `on` month)

groupByWeek :: [Day] -> [[Day]]
groupByWeek = filter (not . null) . split (keepDelimsR $ whenElt isSunday)

charSize, lineSize :: Int
charSize = 2
lineSize = length allDays * charSize + 6

formatWeek :: [Day] -> String
formatWeek days@(d:_)
  | isMonday d = padR lineSize printWeek
  | otherwise  = padL lineSize printWeek
  where
    printDay d = padL charSize (show $ dayNb d)
    printWeek = unwords (fmap printDay days)

formatMonth :: [[Day]] -> [String]
formatMonth m =
  let firstDay = head $ head m
      title = center lineSize $ monthRepr (month firstDay)
      padLine = replicate lineSize ' '
      weekLines = fmap formatWeek m ++ repeat padLine
  in title : replicate lineSize '-' : take 7 weekLines

toWeekLine :: [String] -> String
toWeekLine = (++ "\n") . intercalate "   "


-- | The assembled pipe

yearRange :: Integer -> [Day]
yearRange y = [fromGregorian y 1 1 .. fromGregorian y 12 31]    -- ^ Day implements Enum

calendar :: [Day] -> [String]
calendar =
  groupByMonth                -- list (year) of list (month) of days
    >>> fmap groupByWeek    -- list (year) of list (month) of list (week) of days
    >>> fmap formatMonth    -- list (year) of list (month) of week repr
    >>> chunksOf 3          -- group month by 3     => list (year) of list (chunk) of list (month) of week repr
    >>> fmap transpose      -- transpose each chunk => list (year) of list (line) of list (3 month) of week repr
    >>> concat              -- group back chunks    => list (line) of list (3 month) of week repr
    >>> fmap toWeekLine     -- list (line) of strings (representing 3 weeks - one for each month)


-- | Test function

test :: IO ()
test = (calendar >>> mapM_ putStr) (yearRange 2015)
