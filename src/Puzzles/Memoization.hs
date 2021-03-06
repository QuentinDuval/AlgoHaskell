
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Puzzles.Memoization where

import Control.Arrow
import Control.Monad.ST
import Control.Monad.State
import Data.Function (fix, (&))
import Data.Function.Memoize
import Data.Hashable
import qualified Data.HashMap.Lazy as LazyHM
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as UV
import qualified Tree.NaturalTree as Nat


-- TODO - Try lazy structure to help:
-- * Try to use skew index List
-- * Try Data.Vector.Persistent


{-
-- Memoization in Haskell:
-- * Inspired by http://stackoverflow.com/questions/3208258/memoization-in-haskell
-- * Different solution to the given specific problem
-- * Different attempts at generalizing the memoization pattern
-}


-- | The function to memoize

fun :: Int -> Integer
fun n
  | n < 1     = 1
  | otherwise = max (fromIntegral n) (fun (div n 2) + fun (div n 3) + fun (div n 4))


-- | A pure version supporting memoization
-- Makes use of open recursion

fun' :: (Int -> Integer) -> Int -> Integer
fun' f n
  | n < 1     = 1
  | otherwise = max (fromIntegral n) (f (div n 2) + f (div n 3) + f (div n 4))


-- | Memoization via an infinite list
-- * Good lazyness, very interesting in this example
-- * But bad O(N) random access hurts for big ints

memoList :: Int -> Integer
memoList n = memoized n   -- ^ Remove n and it stays between function calls
  where
    memoized  = fun' (memoTable !!)
    memoTable = fmap memoized [0..]


-- | Memoization via a finite vector
-- * No lazyness, which is bad for this example
-- * But O(1) random access for fast memoization

memoVector :: Int -> Integer
memoVector n = memoized n
  where
    memoized  = fun' ((V.!) memoTable)
    memoTable = V.generate n memoized


-- | Memoization via an infinite tree
-- * Very good lazyness, must have for this example
-- * Fast O(log N) random access
-- On this example, performs much better than the vector due to lazyness

memoTree :: Int -> Integer
memoTree n = memoized n   -- ^ Remove n and it stays between function calls
  where
    memoized  = fun' (memoTable `Nat.at`)
    memoTable = fmap memoized Nat.indices


-- | Memoization via a lazy hash map
-- * Poor lazyness: the values are not computed, but the keys are
-- * Fast random access in nearly O(1)
--
-- It has bad data locality when compared to the vector.
-- It still has some form of lazyness, strict hash map would crash

memoMap :: Int -> Integer
memoMap n = memoized n
  where
    memoized  = fun' ((LazyHM.!) memoTable)
    memoTable = LazyHM.fromList $ fmap (id &&& memoized) [0..n]


-- | Memoization using traditional methods
-- * Via side-effects and storage in associative container
-- * Can be done generatically (if monad is supported)
-- * Imperative instead of declarative, but effective

funM :: (Monad m) => (Int -> m Integer) -> Int -> m Integer
funM f n
  | n < 1     = return 1
  | otherwise = do
      subVals <- mapM f $ div n <$> [2, 3, 4]
      return $ max (fromIntegral n) (sum subVals)

type OpenRecursion m a b = (a -> m b) -> a -> m b

genericMemoMonad :: (Eq a, Hashable a) => (forall m. Monad m => OpenRecursion m a b) -> a -> b
genericMemoMonad f n = memoized n `evalState` HM.empty
  where
    memoized n = do
      cached <- HM.lookup n <$> get
      case cached of
        Just res -> return res
        Nothing  -> do
          res <- f memoized n
          modify (HM.insert n res)
          return res

memoMonad :: Int -> Integer
memoMonad = genericMemoMonad funM


-- | Using the module Data.Function.Memoize
--
-- This module works via recursion:
-- * Memoize the results of (a -> b) by storing results by a
-- * Memoize the results of (a -> b -> c) by storing the (b -> c) results of a
-- * To first store the result of a, it needs to be enumerable (mapping to int)
-- * Then it proceeds by using a kind of infinite binary tree for lookups
--
-- Complexity of lookups:
-- * O(log A) for function with one argument of cardinality A
-- * O(log A + log B) for functions with two arguments of cardinality A and B
-- * With uncurry, the loopup is done by pair: O(log (A * B)) = O(log A + log B)
--
-- This is best used when:
-- * We need all the chached information to be maintained, which is not always
--   the case (example of Fibonacci and Bellman-Ford)
-- * We do not know of any trivial topological sort that would allow us to
--   adopt a bottum-up approach

memoModule :: Int -> Integer
memoModule = memoFix fun'


{-
-- Example of memoization where lazyness is not needed.
-- Problem of "Cut Rod":
-- * You have a rod of lenght L and list of value for a rod of length K
-- * Find the best cuts you can make, that maximize the value
-}

cutRod :: V.Vector Int -> (Int -> Int) -> Int -> Int
cutRod values subProblem l
  | l == 0    = 0
  | otherwise = maximum [ values V.! (k-1) + subProblem (l-k) | k <- [1..l] ]

naiveCutRod :: V.Vector Int -> Int
naiveCutRod values = fix (cutRod values) (V.length values)

memoCutRod :: V.Vector Int -> Int
memoCutRod values = V.last memoTable
  where
    memoized  = cutRod values (memoTable V.!)
    memoTable = V.generate (V.length values + 1) memoized

memoCutRod2 :: V.Vector Int -> Int
memoCutRod2 values = runST $ do
  let len = V.length values
  memoTable <- UV.replicate (len + 1) 0
  forM_ [1 .. len] $ \l -> do
    values <- forM [1..l] $ \k -> do
      subProblem <- UV.read memoTable (l-k)
      return $ values V.! (k-1) + subProblem
    UV.write memoTable l (maximum values)
  UV.read memoTable len


{-
-- Example of memoization where we need intermediary variables.
-- Problem of "0-1 Knapsack":
-- * A bunch of items, each with a value and a size (integer)
-- * A knapsack of bounded size (integer)
-- * The goal is to find the maximize the possible value
--
-- Solution:
-- * Start from the first item, and decide whether you include it or not
-- * We need memoization, but we need to solve subproblems with different S
-- * It would be bad to use the list of items as memo-key, the start index is enough
-}

type Item = (Int, Int)

knapsack :: V.Vector Item -> (Int -> Int -> Int) -> Int -> Int -> Int
knapsack items subProblem startIndex remSize
  | startIndex >= V.length items = 0
  | size < remSize  = max withoutItem (withItem + value)
  | size == remSize = max withoutItem value
  | otherwise       = withoutItem
  where
    (size, value) = items V.! startIndex
    suffixProblem = subProblem (succ startIndex)
    withoutItem   = suffixProblem remSize
    withItem      = suffixProblem (remSize - size)

naiveKnapsack :: V.Vector Item -> Int -> Int
naiveKnapsack items = subProblem 0
  where subProblem = knapsack items subProblem -- ^ Equivalent of "fix"

memoKnapsack :: V.Vector Item -> Int -> Int
memoKnapsack items totalSize = memoTable & V.last & V.head -- ^ "&" is "flip ($)"
  where
    itemCount = V.length items
    memoized  = knapsack items (\start size -> memoTable V.! size V.! start)
    memoTable = V.generate (succ totalSize) (V.generate (succ itemCount) . flip memoized)

memoKnapsack2 :: V.Vector Item -> Int -> Int
memoKnapsack2 items = memoFix2 (knapsack items) 0
