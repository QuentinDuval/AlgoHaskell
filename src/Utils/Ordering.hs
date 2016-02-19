module Utils.Ordering where

import Data.Function

minBy, maxBy :: (a -> a -> Ordering) -> a -> a -> a
minBy comp a b = if comp a b == GT then b else a
maxBy comp a b = if comp a b == LT then b else a

minOn, maxOn :: (Ord b) => (a -> b) -> a -> a -> a
minOn proj = minBy (compare `on` proj)
maxOn proj = maxBy (compare `on` proj)
