module Numerics where

import Data.Monoid


-- | Fast exponentiation algorithm

fastExp :: (Monoid a) => a -> Int -> a
fastExp val e
    | e < 1     = mempty
    | e == 1    = val
    | odd e     = val <> fastExp val (e - 1)
    | otherwise =
        let rec = fastExp val (div e 2)
        in rec <> rec

