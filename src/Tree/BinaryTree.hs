{-# LANGUAGE RecordWildCards  #-}
module Tree.BinaryTree (
  BinaryTree(..),
  leaf, node,
  buildBalanced,
  minDepth, maxDepth,
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
-- Implementation with 2 traversals: O(N) complexity
-- Calling splitAt or length at each level would make it O(N log N)
buildBalanced :: [a] -> BinaryTree a
buildBalanced vals = fst $ build (length vals) vals
  where
    build 0   vals = (Leaf, vals)
    build len vals =
      let mid = div (len - 1) 2
          (ltree, root : rvals) = build mid vals
          (rtree, remainingVal) = build (len - mid - 1) rvals
      in (node root ltree rtree, remainingVal)


-- | Traversal

instance Foldable BinaryTree where
  foldr reduce = visit where
    visit prev Leaf        = prev
    visit prev n@Node{..}  = visit (reduce val (visit prev rhs)) lhs


-- | Utils

maxDepth, minDepth :: BinaryTree a -> Int
maxDepth = getDepth max
minDepth = getDepth min

getDepth :: (Int -> Int -> Int) -> BinaryTree a -> Int
getDepth merger = visit where
  visit Leaf = 0
  visit Node{..} = 1 + merger (visit lhs) (visit rhs)
