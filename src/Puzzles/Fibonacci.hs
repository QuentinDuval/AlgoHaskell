module Puzzles.Fibonacci (
    fibIterate,
    fibImperative,
    fibMatrix,
    fibMatrixTR,
) where

import Control.Monad
import Control.Monad.ST
import Data.STRef.Strict
import Numerics


-- | Implementation based on the function iterate (linear complexity)

fibIterate :: Int -> Integer
fibIterate n = map fst (iterate next (0,1)) !! n
   where next (a, b) = (b, b + a)


-- | Implementation based on the ST monad (linear complexity)

fibImperative :: Int -> Integer
fibImperative n = runST $ do
    a <- newSTRef 0
    b <- newSTRef 1
    forM_ [1..n] $ \_ -> do
        curr <- readSTRef a
        writeSTRef a =<< readSTRef b
        modifySTRef' b (+ curr)
    readSTRef a


-- | Fast exponentiation implementation (logarithmic complexity)

data FibMatrix = FibMatrix Integer Integer Integer Integer

instance Monoid FibMatrix where
    mempty  = FibMatrix 1 0 0 1
    mappend (FibMatrix x11 x12 x21 x22) (FibMatrix y11 y12 y21 y22)
        = FibMatrix
            (x11 * y11 + x12 * y21)
            (x11 * y12 + x12 * y22)
            (x21 * y11 + x22 * y21)
            (x21 * y12 + x22 * y22)

fibRes :: FibMatrix -> Integer
fibRes (FibMatrix _ res _ _) = res

fibMatrix :: Int -> Integer
fibMatrix = fibRes . fastExpNaiveRec (FibMatrix 1 1 1 0)

fibMatrixTR :: Int -> Integer
fibMatrixTR = fibRes . fastExpTailRec (FibMatrix 1 1 1 0)

