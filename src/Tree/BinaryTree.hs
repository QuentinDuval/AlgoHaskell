module Tree.BinaryTree (
  BinaryTree,
  buildBalanced,
) where


-- | Binary tree

data BinaryTree a
  = Leaf
  | Node {
    val :: a,
    lhs, rhs :: BinaryTree a
  } deriving (Show, Eq, Ord)


-- | Smart constructors

leaf :: a -> BinaryTree a
leaf a = node a Leaf Leaf

node :: a -> BinaryTree a -> BinaryTree a -> BinaryTree a
node = Node

data Nat t = Zero | One t | Two t t

buildBalanced :: [a] -> BinaryTree a
buildBalanced = undefined

-- TODO: The merge is complex since it needs more values to be performed

-- buildBalanced = go []
--   where
--     go :: [Nat (BinaryTree a)] -> [a] -> BinaryTree a
--     go []  (t:ts) = go [One $ leaf t] ts
--     go acc []     = undefined

    -- push :: [Nat (BinaryTree a)] -> [a] -> [Nat (BinaryTree a)]
    -- push (Zero : cs) (v:vs)       = push (One   (leaf v) : cs) vs
    -- push (One c: cs) (v:vs)       = push (Two c (leaf v) : cs) vs
    -- push (Two c1 c2 : cs) (v:vs)  = Zero : push (One (node v c1 c2) : cs) vs


-- | Accessors

-- getRank :: BinaryTree a -> Int
-- getRank Leaf      = 0
-- getRank n@Node{}  = rank n

getValue :: BinaryTree a -> a
getValue = val
