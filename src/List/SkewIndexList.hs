{-# LANGUAGE RecordWildCards #-}
module List.SkewIndexList (
  IndexedList,
  module Class,
) where

import List.IndexListClass as Class


{-
Skew binary number implementation of random access list (based on Okasaki's book)
Based on skew binary numbers: (2^(i+1)) - 1

Complexity:
* List operations in O(1)
* Length retrieval in O(log N)
* Random access in O(log N)
* Traversal in O(N)
-}

-- TODO: Based on it, provide a vector like data structure (with push back)
-- TODO: eliminate redundancy in 'at' and 'tree lookups'

data Tree a
  = Leaf { nodeVal  :: a }
  | Node { nodeVal  :: a,
           lhs, rhs :: Tree a }
  deriving (Show, Eq, Ord)

type TreeSize = Int

data DigitTree a
  = DigitTree {
    digitSize :: TreeSize,
    digitTree :: Tree a }
  deriving (Show, Eq, Ord)

type Digits a = [DigitTree a]

newtype IndexedList a = IndexedList {
  digits :: Digits a
} deriving (Show, Eq, Ord)


-- | Public

instance IIndexList IndexedList where
  empty         = IndexedList []
  pushFront a   = IndexedList . pushVal a . digits
  getTail       = IndexedList . popVal . digits
  getHead       = topVal . digits
  at            = lookupList . digits
  updateAt f l  = IndexedList . updateList f (digits l)


-- | Useful instances

instance Functor Tree where
  fmap fct Leaf{..} = Leaf (fct nodeVal)
  fmap fct Node{..} = Node (fct nodeVal) (fmap fct lhs) (fmap fct rhs)

instance Functor IndexedList where
  fmap fct = IndexedList . fmap mapDigit . digits
    where mapDigit d@(DigitTree _ t) = d { digitTree = fmap fct t }

instance Foldable Tree where
  foldr fct prev Leaf{..} = fct nodeVal prev
  foldr fct prev Node{..} = fct nodeVal (foldr fct (foldr fct prev rhs) lhs)

instance Foldable IndexedList where
  length = sum . fmap digitSize . digits
  foldr fct prev l = foldr foldDigits prev (digits l)
    where foldDigits d prev = foldr fct prev (digitTree d)


-- | Private (utils)

leaf :: a -> DigitTree a
leaf = DigitTree 1 . Leaf

linkNodes :: a -> DigitTree a -> DigitTree a -> DigitTree a
linkNodes v d1 d2 = DigitTree {
    digitSize = 1 + digitSize d1 + digitSize d2,
    digitTree = Node v (digitTree d1) (digitTree d2)
  }


-- | Private (indexation)

indexError i = error ("Error: index out of bound: " ++ show i)

lookupList :: Digits a -> Int -> a
lookupList [] i = indexError i
lookupList (d:ds) i
  | i >= dSize           = lookupList ds (i - dSize)
  | otherwise            = lookupNode dSize (digitTree d) i
  where dSize = digitSize d

lookupNode :: TreeSize -> Tree a -> Int -> a
lookupNode 1 Leaf{..}   0 = nodeVal
lookupNode s n@Node{..} i
  | i == 0     = nodeVal
  | i <= lSize = lookupNode lSize lhs (i - 1)
  | otherwise  = lookupNode lSize rhs (i - lSize - 1)
  where lSize = div s 2

updateList :: (a -> a) -> Digits a -> Int -> Digits a
updateList _ [] i = indexError i
updateList f (d:ds) i
  | i >= dSize = d : updateList f ds (i - dSize)
  | otherwise  = d { digitTree = updateNode f dSize (digitTree d) i } : ds
  where dSize = digitSize d

updateNode :: (a -> a) -> TreeSize -> Tree a -> Int -> Tree a
updateNode f 1 Leaf{..}   0 = Leaf (f nodeVal)
updateNode f s n@Node{..} i
  | i == 0     = n { nodeVal = f nodeVal }
  | i <= lSize = n { lhs = updateNode f lSize lhs (i - 1) }
  | otherwise  = n { rhs = updateNode f lSize rhs (i - lSize - 1) }
  where lSize = div s 2


-- | Private (list operations)

pushVal :: a -> Digits a -> Digits a
pushVal v []  = [leaf v]
pushVal v [d] = [leaf v, d]
pushVal v ds@(d1:d2:rest)
  | digitSize d1 == digitSize d2  = linkNodes v d1 d2 : rest
  | otherwise                     = leaf v : ds

topVal :: Digits a -> a
topVal []     = error "Error (topVal): list is empty"
topVal (d:_)  = nodeVal (digitTree d)

popVal :: Digits a -> Digits a
popVal []                   = error "Error (popVal): list is empty"
popVal (DigitTree 1 _ : ds) = ds
popVal (DigitTree s t : ds) = DigitTree s' (lhs t) : DigitTree s' (rhs t) : ds
  where s' = div s 2
