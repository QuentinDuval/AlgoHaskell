{-# LANGUAGE RecordWildCards #-}
module List.IndexList (
  IndexedList,
  empty,
  fromList,
  pushFront,
  popFront,
  getFirst,
  at,
  updateAt,
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

instance Functor Tree where
  fmap fct (Leaf v) = Leaf (fct v)
  fmap fct Node{..} = Node nodeSize (fmap fct lhs) (fmap fct rhs)

instance Foldable Tree where
  foldr fct prev (Leaf v) = fct v prev
  foldr fct prev Node{..} = foldr fct (foldr fct prev rhs) lhs


-- ^ Binary counter on trees, with two for efficiency

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


-- | The index list implementation
--
-- TODO: Based on this implementation:
-- * Provide a vector like data structure (with push back)
-- * Find a way to efficiently merge the lists
-- * Make it traversable
-- TODO: eliminate redundancy in 'at' and 'tree lookups'

type Digits a = [DigitTree a]

newtype IndexedList a = IndexedList {
  digits :: Digits a
} deriving (Show, Eq, Ord)


-- | Public

empty :: IndexedList a
empty = IndexedList []

fromList :: [a] -> IndexedList a
fromList = foldr pushFront empty

pushFront :: a -> IndexedList a -> IndexedList a
pushFront a = IndexedList . pushTree (Leaf a) . digits

popFront :: IndexedList a -> IndexedList a
popFront = IndexedList . snd . popTree . digits

getFirst :: IndexedList a -> a
getFirst = leafVal . fst . popTree . digits

at :: IndexedList a -> Int -> a
at = lookupList . digits

updateAt :: (a -> a) -> IndexedList a -> Int -> IndexedList a
updateAt f l = IndexedList . updateList f (digits l)


-- | Useful instances

instance Functor IndexedList where
  fmap fct = IndexedList . fmap (fmap fct) . digits

instance Foldable IndexedList where
  length = sum . fmap digitSize . digits
  foldr fct prev l = foldr foldDigits prev (digits l)
    where foldDigits = flip (foldr fct)


-- | Private (indexation)

indexError = error "Error: index out of bound"

lookupList :: Digits a -> Int -> a
lookupList [] _ = indexError
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
updateList _ [] _ = indexError
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
