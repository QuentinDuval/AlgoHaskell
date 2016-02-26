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

-- First implementation with 2 traversals: O(N) complexity
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

-- Second implementation with only one traversal: O(N) complexity
-- Using skew binary numbers to build the tree
-- TODO: does not seem to be working...

buildBalancedSkew :: [a] -> BinaryTree a
buildBalancedSkew = go []
  where
    go acc [] = undefined -- foldl (node ???) Leaf $ fmap fst acc
    go ((t1, r1):(t2, r2):ts) (x:xs)
      | r1 == r2 = go ((node x t1 t2, succ r1) : ts) xs
    go ts (x:xs) = go ((leaf x, 1) : ts) xs


-- | Traversal

instance Foldable BinaryTree where
  foldr reduce = visit where
    visit prev Leaf        = prev
    visit prev n@Node{..}  = visit (reduce val (visit prev rhs)) lhs


-- | Rotate Left
--   (a)
--  /  \
-- x   (c)
--     / \
--   (b)  y
--
--      (c)
--     /  \
--   (a)   y
--  /  \
-- x   (b)

rotateL :: BinaryTree a -> BinaryTree a
rotateL a@Node{ rhs = c@Node{ lhs = b } } = c { lhs = a { rhs = b } }

rotateR :: BinaryTree a -> BinaryTree a
rotateR c@Node{ lhs = a@Node{ rhs = b } } = a { rhs = c { lhs = b } }


-- | Utils

maxDepth, minDepth :: BinaryTree a -> Int
maxDepth = getDepth max
minDepth = getDepth min

getDepth :: (Int -> Int -> Int) -> BinaryTree a -> Int
getDepth merger = visit where
  visit Leaf = 0
  visit Node{..} = 1 + merger (visit lhs) (visit rhs)
