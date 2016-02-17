module Algo.Folds (
  rankFold
) where


{-
- Implements a binary counter to match and merge elements of same "merge rank"
- @requires that inputs are ordered in decreasing size
-}
rankFold :: (a -> Int) -> (a -> a -> a) -> [a] -> a
rankFold rank merge rmqs = go [head rmqs] (tail rmqs)
  where
    go acc []     = foldr1 (flip merge) acc
    go []  (r:rs) = go [r] rs
    go acc (r:rs)
      | rank (head acc) > rank r = go (r : acc) rs
      | otherwise                = go (tail acc) (merge (head acc) r : rs)

-- TODO: add a balanced fold not based on ranks and assuming equal sizes at start
