
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Puzzles.Memoization where

import Control.Monad.State
import Data.Function (fix)
import Data.Function.Memoize
import qualified Data.IntMap as IM
import qualified Data.IntTrie as T
import qualified Data.Map as OM
import qualified Tree.NaturalTree as Nat


{-
Memoization in Haskell:
http://stackoverflow.com/questions/3208258/memoization-in-haskell
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
-- If you remove the argument of memoList, memoization stays between function calls

memoList :: Int -> Integer
memoList n = memoized n
  where
    memoized  = fun' (memoTable !!)
    memoTable = fmap memoized [0..]


-- | Memoization via an infinite trees
-- Good lazyness and log N random access
-- If you remove the argument of memoList, memoization stays between function calls

memoTrie :: Int -> Integer
memoTrie n = memoized n
  where
    memoized  = fun' (memoTable `T.apply`)
    memoTable = fmap memoized T.identity

memoTree :: Int -> Integer
memoTree n = memoized n
  where
    memoized  = fun' (memoTable `Nat.at`)
    memoTable = fmap memoized Nat.indices


-- | Memoization using traditional methods
-- Controled side-effects and associative containers

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


-- | Generalization of memoization
-- Attempt at doing it manually

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

testMemoizeMod :: Int -> IO ()
testMemoizeMod = print . memoFix fun'
