
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Puzzles.Memoization where

import Control.Monad.State
import Data.Function (fix)
import Data.Function.Memoize
import qualified Data.IntMap as IM
import qualified Data.Map as OM
import qualified Data.Vector as V
import qualified Tree.NaturalTree as Nat


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
-- Good lazyness but bad random access

memoList :: Int -> Integer
memoList n = memoized n   -- ^ Remove n and it stays between function calls
  where
    memoized  = fun' (memoTable !!)
    memoTable = fmap memoized [0..]


-- | Memoization via a finite vector
-- No lazyness but O(1) random access

memoVector :: Int -> Integer
memoVector n = memoized n
  where
    memoized  = fun' ((V.!) memoTable)
    memoTable = V.generate n memoized


-- | Memoization via an infinite tree
-- Good lazyness and log N random access

memoTree :: Int -> Integer
memoTree n = memoized n   -- ^ Remove n and it stays between function calls
  where
    memoized  = fun' (memoTable `Nat.at`)
    memoTable = fmap memoized Nat.indices


-- | Memoization using traditional methods
-- * Via side-effects and storage in associative containers
-- * Can be done generatically (example shown below)

funM :: (Monad m) => (Int -> m Integer) -> Int -> m Integer
funM f n
  | n < 1     = return 1
  | otherwise = do
      subVals <- mapM f $ div n <$> [2, 3, 4]
      return $ max (fromIntegral n) (sum subVals)

memoMonad :: Int -> Integer
memoMonad n = memoized n `evalState` IM.empty
  where
    memoized :: Int -> State (IM.IntMap Integer) Integer
    memoized n = do
      cached <- IM.lookup n <$> get
      case cached of
        Just res -> return res
        Nothing  -> do
          res <- funM memoized n
          modify (IM.insert n res)
          return res

-- Generalized version

type OpenRecursion m a b = (a -> m b) -> a -> m b

memoRecur :: (Ord a) => (forall m. Monad m => OpenRecursion m a b) -> a -> b
memoRecur f n = memoized n `evalState` OM.empty
  where
    memoized n = do
      cached <- OM.lookup n <$> get
      case cached of
        Just res -> return res
        Nothing  -> do
          res <- f memoized n
          modify (OM.insert n res)
          return res

testMemoRecur :: Int -> IO ()
testMemoRecur = print . memoRecur funM


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

testMemoizeMod :: Int -> IO ()
testMemoizeMod = print . memoFix fun'
