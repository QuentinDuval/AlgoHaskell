module Puzzles.Fibonacci (
    fibIterate,
    fibMatrix,
    fibMatrixTR,
) where

import Numerics


-- | Implementation based on the function iterate (linear complexity)

fibIterate :: Int -> Integer
fibIterate n = map fst (iterate next (0,1)) !! n
   where next (a, b) = (b, b + a)


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

fibMatrixImpl ::  (FibMatrix -> Int -> FibMatrix) -> Int -> Integer
fibMatrixImpl fastExpImpl n =
    let (FibMatrix _ res _ _) = fastExpImpl (FibMatrix 1 1 1 0) n
    in res

fibMatrix :: Int -> Integer
fibMatrix = fibMatrixImpl fastExpNaiveRec

fibMatrixTR :: Int -> Integer
fibMatrixTR = fibMatrixImpl fastExpTailRec

