{-# LANGUAGE BangPatterns #-}
module Numerics (
    fastExp,
    fastExpTailRec,
    fastExpNaiveRec,
) where

import Data.Monoid


-- | Fast exponentiation algorithms

fastExp :: (Monoid a) => a -> Int -> a
fastExp = fastExpTailRec

fastExpTailRec :: (Monoid a) => a -> Int -> a -- TODO unwarp the monoid instance, it seems to be slowing things down
fastExpTailRec = recur mempty
    where
        recur !acc !val e
            | e <= 0    = acc
            | odd e     = recur (val <> acc) val (e - 1)
            | otherwise = recur acc (val <> val) (div e 2)

fastExpNaiveRec :: (Monoid a) => a -> Int -> a
fastExpNaiveRec val e
    | e <= 0    = mempty
    | odd e     = val <> res <> res
    | otherwise = res <> res
    where res = fastExpNaiveRec val (div e 2)

