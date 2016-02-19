module Utils.Time (
  module Utils.Time,
  module Exports,
) where

import Data.Time as Exports
import Utils.List


-- | Time utils

year :: Day -> Integer
year d = let (y, _, _) = toGregorian d in y

month :: Day -> Int
month d = let (_, m, _) = toGregorian d in m

dayNb :: Day -> Int
dayNb d = let (_, _, n) = toGregorian d in n

day :: Day -> String
day = formatTime defaultTimeLocale "%a"

allDays :: [String]
allDays = rotate 1 (snd <$> wDays defaultTimeLocale)

monthRepr :: Int -> String
monthRepr n = fst $ months defaultTimeLocale !! (n - 1)

isMonday, isSunday :: Day -> Bool
isMonday d = day d == head allDays
isSunday d = day d == last allDays
