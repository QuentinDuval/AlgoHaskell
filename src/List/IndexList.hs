{-# LANGUAGE RecordWildCards #-}
module List.IndexList (
  IndexedList,
  module Class,
) where

import List.IndexListClass as Class


{-
First naive implementation of random access list (based on Okasaki's book)
Based on traditional binary numbers: 2 ^ i

Complexity:
* List operations in O(log N)
* Length retrieval in O(log N)
* Random access in O(log N)
* Traversal in O(N)

Issues:
* Binary numbers have cascading effects (a carry or borrow propagate)
* Inefficiency by keeping track of the size at each node
* Inefficiency by keeping track of the zeros (useless as we have the sizes)
-}

-- TODO: Based on it, provide a vector like data structure (with push back)
-- TODO: eliminate redundancy in 'at' and 'tree lookups'


-- ^ A complete binary tree

data Tree a
  = Leaf { leafVal  :: a }
  | Node { nodeSize :: Int,
           lhs, rhs :: Tree a }
  deriving (Show, Eq, Ord)

treeSize :: Tree a -> Int
treeSize Leaf{}   = 1
treeSize n@Node{} = nodeSize n

linkNodes :: Tree a -> Tree a -> Tree a
linkNodes l r = Node (treeSize l + treeSize r) l r

instance Functor Tree where
  fmap fct (Leaf v) = Leaf (fct v)
  fmap fct Node{..} = Node nodeSize (fmap fct lhs) (fmap fct rhs)

instance Foldable Tree where
  foldr fct prev (Leaf v) = fct v prev
  foldr fct prev Node{..} = foldr fct (foldr fct prev rhs) lhs


-- ^ Binary counter on trees, with two added for efficiency

data DigitTree a
  = Zero
  | One { one      :: Tree a }
  | Two { one, two :: Tree a }
  deriving (Show, Eq, Ord)

digitSize :: DigitTree a -> Int
digitSize Zero      = 0
digitSize (One t)   = treeSize t
digitSize (Two f s) = treeSize f + treeSize s

instance Functor DigitTree where
  fmap fct Zero       = Zero
  fmap fct (One f)    = One (fmap fct f)
  fmap fct (Two f s)  = Two (fmap fct f) (fmap fct s)

instance Foldable DigitTree where
  foldr fct prev Zero      = prev
  foldr fct prev (One f)   = foldr fct prev f
  foldr fct prev (Two f s) = foldr fct (foldr fct prev s) f


-- ^ The index list implementation

type Digits a = [DigitTree a]

newtype IndexedList a = IndexedList {
  digits :: Digits a
} deriving (Show, Eq, Ord)


-- | Public

instance IIndexList IndexedList where
  empty         = IndexedList []
  pushFront a   = IndexedList . pushTree (Leaf a) . digits
  getTail       = IndexedList . snd . popTree . digits
  getHead       = leafVal . fst . popTree . digits
  at            = lookupList . digits
  updateAt f l  = IndexedList . updateList f (digits l)

instance Functor IndexedList where
  fmap fct = IndexedList . fmap (fmap fct) . digits

instance Foldable IndexedList where
  length = sum . fmap digitSize . digits
  foldr fct prev l = foldr foldDigits prev (digits l)
    where foldDigits = flip (foldr fct)


-- | Private (indexation)

indexError i = error ("Error: index out of bound: " ++ show i)

lookupList :: Digits a -> Int -> a
lookupList [] i = indexError i
lookupList (d:ds) i
  | i >= dSize           = lookupList ds (i - dSize)
  | i < treeSize (one d) = lookupNode (one d) i
  | otherwise            = lookupNode (two d) i
  where dSize = digitSize d

lookupNode :: Tree a -> Int -> a
lookupNode Leaf{..} _ = leafVal
lookupNode Node{..} i
  | i < lSize = lookupNode lhs i
  | otherwise = lookupNode rhs (i - lSize)
  where lSize = treeSize lhs

updateList :: (a -> a) -> Digits a -> Int -> Digits a
updateList _ [] i = indexError i
updateList f (d:ds) i
  | i >= dSize           = d : updateList f ds (i - dSize)
  | i < treeSize (one d) = d { one = updateNode f (one d) i } : ds
  | otherwise            = d { two = updateNode f (two d) i } : ds
  where dSize = digitSize d

updateNode :: (a -> a) -> Tree a -> Int -> Tree a
updateNode f Leaf{..}   _ = Leaf (f leafVal)
updateNode f n@Node{..} i
  | i < lSize = n { lhs = updateNode f lhs i }
  | otherwise = n { rhs = updateNode f rhs (i - lSize) }
  where lSize = treeSize lhs


-- | Private (list operations)

pushTree :: Tree a -> Digits a -> Digits a
pushTree t []             = [One t]
pushTree t (Zero : ts)    = One t : ts
pushTree t (One f : ts)   = Two t f : ts
pushTree t (Two f s : ts) = One t : pushTree (linkNodes f s) ts

popTree :: Digits a -> (Tree a, Digits a)
popTree [] = error "Error (popTree): list is empty"
popTree [One f]           = (f, [])
popTree (One f : ts)      = (f, Zero : ts)
popTree (Two f s : ts)    = (f, One s : ts)
popTree (Zero : ts)       = (l, One r : ts')
  where (Node _ l r, ts') = popTree ts
