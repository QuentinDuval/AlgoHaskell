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


-- | Construction of a balanced binary tree

-- ^ Naive implementation with O(log N) traversals
buildBalancedNaive :: [a] -> BinaryTree a
buildBalancedNaive vals = build (length vals) vals
  where
    build _   []   = Leaf
    build len vals =
      let mid = div (len - 1) 2
          (left, root : right) = splitAt mid vals
      in node root (build mid left) (build (len - mid - 1) right)

-- ^ Better implementation with 2 traversals
buildBalanced :: [a] -> BinaryTree a
buildBalanced vals = fst $ build (length vals) vals
  where
    build 0   vals = (Leaf, vals)
    build len vals =
      let mid = div (len - 1) 2
          (ltree, root : rvals) = build mid vals
          (rtree, remainingVal) = build (len - mid - 1) rvals
      in (node root ltree rtree, remainingVal)


-- | Accessors

-- getRank :: BinaryTree a -> Int
-- getRank Leaf      = 0
-- getRank n@Node{}  = rank n

getValue :: BinaryTree a -> a
getValue = val
