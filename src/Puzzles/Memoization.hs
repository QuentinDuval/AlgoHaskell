module Puzzles.Memoization where

import Control.Monad.State
import Data.Function (fix)
import qualified Data.IntMap as M
import qualified Data.IntTrie as T
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
memoMonad n = memoized n `evalState` M.empty
  where
    memoized :: Int -> State (M.IntMap Integer) Integer
    memoized n = do
      cached <- M.lookup n <$> get
      case cached of
        Just res -> return res
        Nothing  -> do
          res <- funM memoized n
          modify (M.insert n res)
          return res
