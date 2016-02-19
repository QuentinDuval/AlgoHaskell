{-# LANGUAGE RecordWildCards #-}
module Tree.NaturalTree (
  NaturalTree,
  at,
  indices,
) where


{-
-- Infinite tree, with a mapping to the natural numbers
-- Best used in place of infinite lists, when random access is needed
--
-- It follows the indexation scheme:
--
--             0
--      1            2
--   3    5       4    6
-- 7 11  9 13   8 12  10 14
--
-- Starting from an index i:
-- * If i is odd, then go left, and divide i by 2
-- * Otherwise, go right, and divide i by 2 and remove 1
--
-- To generate the indices at current depth d:
-- * To left,  add 2 ^ d
-- * To right, add 2 ^ (d+1)
-- Example with 3 (at depth 2): add 4 to get 7, add 8 to get 11
-}

data NaturalTree a
  = NaturalTree {
    value :: a,
    lhs, rhs :: NaturalTree a
  }

at :: NaturalTree a -> Int -> a
at NaturalTree{..} 0 = value
at NaturalTree{..} i
  | odd i     = at lhs (div i 2)
  | otherwise = at rhs (div i 2 - 1)

indices :: (Integral i) => NaturalTree i
indices = build 0 1
  where
    build current currPow =
      let nextPow = 2 * currPow
      in NaturalTree {
        value = current,
        lhs = build (current + currPow) nextPow,  -- ^ Add 2 ^ depth
        rhs = build (current + nextPow) nextPow   -- ^ Add 2 ^ depth + 1
      }

instance Functor NaturalTree where
  fmap fct NaturalTree{..}
    = NaturalTree {
      value = fct value,
      lhs = fmap fct lhs,
      rhs = fmap fct rhs
    }
