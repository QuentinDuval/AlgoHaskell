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
buildBalanced vals = build (length vals) vals
  where
    build _   []   = Leaf
    build len vals =
      let mid = div (len - 1) 2
          (left, root : right) = splitAt mid vals
      in node root (build mid left) (build (len - mid - 1) right)


-- | Accessors

-- getRank :: BinaryTree a -> Int
-- getRank Leaf      = 0
-- getRank n@Node{}  = rank n

getValue :: BinaryTree a -> a
getValue = val
