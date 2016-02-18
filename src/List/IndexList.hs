{-# LANGUAGE RecordWildCards #-}
module List.IndexList (
  IndexedList,
  pushFront,
  popFront,
  getFirst,
  getLength,
  at,
) where


-- ^ A complete binary tree

data Tree a
  = Leaf { leafVal :: a }
  | Node {
    nodeSize :: Int,
    lhs, rhs :: Tree a }
  deriving (Show, Eq, Ord)

treeSize :: Tree a -> Int
treeSize Leaf{}   = 1
treeSize n@Node{} = nodeSize n

linkNodes :: Tree a -> Tree a -> Tree a
linkNodes l r = Node (treeSize l + treeSize r) l r


-- ^ Binary counter on trees, with two for efficiency

data DigitTree a
  = Zero
  | One { one      :: Tree a }
  | Two { one, two :: Tree a }
  deriving (Show, Eq, Ord)

digitSize :: DigitTree a -> Int
digitSize Zero = 0
digitSize (One t) = treeSize t
digitSize (Two f s) = treeSize f + treeSize s


-- | The index list implementation
--
-- TODO: Based on this implementation:
-- * Provide a vector like data structure (with push back)
-- * Find a way to efficiently merge the lists

type IndexedList a = [DigitTree a]


-- | Public

pushFront :: a -> IndexedList a -> IndexedList a
pushFront a = pushTree (Leaf a)

popFront :: IndexedList a -> IndexedList a
popFront = snd . popTree

getFirst :: IndexedList a -> a
getFirst = leafVal . fst . popTree

getLength :: IndexedList a -> Int
getLength = sum . fmap digitSize

at :: IndexedList a -> Int -> a
at [] _ = indexError
at (d:ds) i
  | i >= dSize           = at ds (i - dSize)
  | i < treeSize (one d) = lookupNode (one d) i
  | otherwise            = lookupNode (two d) i
  where dSize = digitSize d

updateAt :: IndexedList a -> Int -> (a -> a) -> IndexedList a
updateAt [] _ _ = indexError
updateAt (d:ds) i f
  | i >= dSize           = d : updateAt ds (i - dSize) f
  | i < treeSize (one d) = d { one = updateNode (one d) i f } : ds
  | otherwise            = d { two = updateNode (two d) i f } : ds
  where dSize = digitSize d

-- TODO: eliminate redundancy in 'at' and 'tree lookups'


-- | Private

indexError = error "Error: index out of bound"

lookupNode :: Tree a -> Int -> a
lookupNode Leaf{..} _ = leafVal
lookupTree Node{..} i
  | i < lSize = lookupTree lhs i
  | otherwise = lookupTree rhs (i - lSize)
  where lSize = treeSize lhs

updateNode :: Tree a -> Int -> (a -> a) -> Tree a
updateNode Leaf{..}   _ f = Leaf (f leafVal)
updateNode n@Node{..} i f
  | i < lSize = n { lhs = updateNode lhs i f }
  | otherwise = n { rhs = updateNode rhs (i - lSize) f }
  where lSize = treeSize lhs

pushTree :: Tree a -> IndexedList a -> IndexedList a
pushTree t []             = [One t]
pushTree t (Zero : ts)    = One t : ts
pushTree t (One f : ts)   = Two t f : ts
pushTree t (Two f s : ts) = One t : pushTree (linkNodes f s) ts

popTree :: IndexedList a -> (Tree a, IndexedList a)
popTree [] = error "Error (popTree): list is empty"
popTree [One f]           = (f, [])
popTree (One f : ts)      = (f, Zero : ts)
popTree (Two f s : ts)    = (f, One s : ts)
popTree (Zero : ts)       = (l, One r : ts')
  where (Node _ l r, ts') = popTree ts
