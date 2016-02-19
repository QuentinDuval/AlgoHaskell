module List.SkewIndexList (
  IndexedList,
) where




-- ^ Skew tree (containing 2^i - 1 elements)

data Tree a
  = Leaf { nodeVal  :: a }
  | Node { nodeVal  :: a,
           lhs, rhs :: Tree a }
  deriving (Show, Eq, Ord)

data DigitTree a
  = DigitTree {
    digitSize :: Int,
    digitTree :: Tree a }
  deriving (Show, Eq, Ord)


-- ^ The index list representation

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
getFirst = undefined -- leafVal . fst . popTree . digits

at :: IndexedList a -> Int -> a
at = lookupList . digits

updateAt :: (a -> a) -> IndexedList a -> Int -> IndexedList a
updateAt f l = IndexedList . updateList f (digits l)


-- | Useful instances

instance Functor IndexedList where
  fmap fct = undefined -- IndexedList . fmap (fmap fct) . digits

instance Foldable IndexedList where
  length = sum . fmap digitSize . digits
  foldr fct prev l = undefined --foldr foldDigits prev (digits l)
    --where foldDigits = flip (foldr fct)


-- | Private (indexation)

indexError = error "Error: index out of bound"

lookupList :: Digits a -> Int -> a
lookupList [] _ = indexError
lookupList (d:ds) i
  | i >= dSize           = lookupList ds (i - dSize)
  | otherwise            = lookupNode (digitTree d) i
  where dSize = digitSize d

lookupNode :: Tree a -> Int -> a
lookupNode = undefined
-- lookupNode Leaf{..} _ = leafVal
-- lookupNode Node{..} i
--   | i < lSize = lookupNode lhs i
--   | otherwise = lookupNode rhs (i - lSize)
--   where lSize = treeSize lhs

updateList :: (a -> a) -> Digits a -> Int -> Digits a
updateList _ [] _ = indexError
updateList f (d:ds) i
  | i >= dSize = d : updateList f ds (i - dSize)
  | otherwise  = d { digitTree = updateNode f (digitTree d) i } : ds
  where dSize = digitSize d

updateNode :: (a -> a) -> Tree a -> Int -> Tree a
updateNode = undefined
-- updateNode f Leaf{..}   _ = Leaf (f leafVal)
-- updateNode f n@Node{..} i
--   | i < lSize = n { lhs = updateNode f lhs i }
--   | otherwise = n { rhs = updateNode f rhs (i - lSize) }
--   where lSize = treeSize lhs


-- | Private (list operations)

pushTree :: Tree a -> Digits a -> Digits a
pushTree = undefined
-- pushTree t []             = [One t]
-- pushTree t (Zero : ts)    = One t : ts
-- pushTree t (One f : ts)   = Two t f : ts
-- pushTree t (Two f s : ts) = One t : pushTree (linkNodes f s) ts

popTree :: Digits a -> (Tree a, Digits a)
popTree = undefined
-- popTree [] = error "Error (popTree): list is empty"
-- popTree [One f]           = (f, [])
-- popTree (One f : ts)      = (f, Zero : ts)
-- popTree (Two f s : ts)    = (f, One s : ts)
-- popTree (Zero : ts)       = (l, One r : ts')
--   where (Node _ l r, ts') = popTree ts
