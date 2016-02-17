module Algo.Folds (
  rankFold
) where


{-
- Implements a binary counter to match and merge elements of same "merge rank"
- @requires that inputs are ordered in decreasing size
-}
rankFold :: (a -> Int) -> (a -> a -> a) -> [a] -> a
rankFold rank merge = go []
  where
    go []  (r:rs) = go [r] rs                 -- ^ Start: no binary counter
    go acc []     = foldr1 (flip merge) acc   -- ^ End:   merge counter
    go acc@(c:cs) (r:rs)                      -- ^ Combine equal ranks
      | rank c > rank r = go (r : acc) rs
      | rank r > rank c = error "Inputs are not ordered"
      | otherwise       = go cs (merge c r : rs)

-- TODO: add a balanced fold not based on ranks and assuming equal sizes at start
