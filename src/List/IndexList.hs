module List.IndexList (
) where



-- ^ Use to implement implicit recursive slowdown

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

node :: Tree a -> Tree a -> Tree a
node l r = Node (treeSize l + treeSize r) l r


-- ^ Binary counter on trees, with two for efficiency
data DigitTree a
  = Zero
  | One { one      :: Tree a }
  | Two { one, two :: Tree a }
  deriving (Show, Eq, Ord)


-- ^ The index list itself
type IndexedList a = [DigitTree a]


-- | List operations

pushFront :: a -> IndexedList a -> IndexedList a
pushFront a = pushTree (Leaf a)

popFront :: IndexedList a -> IndexedList a
popFront = snd . popTree

getFirst :: IndexedList a -> a
getFirst = leafVal . fst . popTree


-- | Private

pushTree :: Tree a -> IndexedList a -> IndexedList a
pushTree t []             = [One t]
pushTree t (Zero : ts)    = One t : ts
pushTree t (One f : ts)   = Two t f : ts
pushTree t (Two f s : ts) = One t : pushTree (node f s) ts

popTree :: IndexedList a -> (Tree a, IndexedList a)
popTree [] = error "Error (popTree): list is empty"
popTree [One f]           = (f, [])
popTree (One f : ts)      = (f, Zero : ts)
popTree (Two f s : ts)    = (f, One s : ts)
popTree (Zero : ts)       = (l, One r : ts')
  where (Node _ l r, ts') = popTree ts


-- | Indexing into the list

at :: IndexedList a -> Int -> a
at = undefined
