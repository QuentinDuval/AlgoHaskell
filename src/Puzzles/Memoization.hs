module Puzzles.Memoization where

import Data.Function (fix)
import Data.IntTrie
import qualified Tree.NaturalTree as Nat


{-
Memoization in Haskell:
http://stackoverflow.com/questions/3208258/memoization-in-haskell
-}


-- | The function to memoize

fun :: Int -> Integer
fun n
  | n < 1 = 1
  | otherwise = max (fromIntegral n) (fun (div n 2) + fun (div n 3) + fun (div n 4))


-- | A pure version supporting memoization
-- Makes use of open recursion

fun' :: (Int -> Integer) -> Int -> Integer
fun' f n
  | n < 1 = 1
  | otherwise = max (fromIntegral n) (f (div n 2) + f (div n 3) + f (div n 4))


-- | Memoization via an infinite list
-- Good lazyness but bad random access
-- If you remove the argument of memoList, memoization stays between function calls

memoList :: Int -> Integer
memoList n = memoized n
  where
    memoized  = fun' (memoTable !!)
    memoTable = fmap memoized [0..]


-- | Memoization via an infinite int-trie
-- Good lazyness and log N random access
-- If you remove the argument of memoList, memoization stays between function calls

memoTrie :: Int -> Integer
memoTrie n = memoized n
  where
    memoized  = fun' (memoTable `apply`)
    memoTable = fmap memoized identity

memoTree :: Int -> Integer
memoTree n = memoized n
  where
    memoized  = fun' (memoTable `Nat.at`)
    memoTable = fmap memoized Nat.indices
