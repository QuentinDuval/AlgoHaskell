{-# LANGUAGE RecordWildCards #-}
module RMQ.RangeQuery (
    RMQ,
    rangeSize,
    rangeFromList,
    pushBack,
    updateVal,
    elementAt,
    rangeQuery
) where


import Data.Monoid


-- | Abstract data type for range queries

data RMQ a
    = Leaf
    | Node {
        treeSize        :: Int,
        nodeVal         :: a,
        lhs, rhs        :: RMQ a }
    deriving (Show, Eq, Ord)


-- | Public functions

rangeSize :: RMQ a -> Int
rangeSize Leaf = 0
rangeSize rmq  = treeSize rmq

rangeFromList :: (Monoid a) => [a] -> RMQ a
rangeFromList = balancedFold mergeTree . fmap newLeaf

pushBack :: (Monoid a) => RMQ a -> a -> RMQ a
pushBack Leaf a = newLeaf a
pushBack n@Node{..} a
    | rangeSize lhs > rangeSize rhs = mergeTree lhs (pushBack rhs a)
    | otherwise                     = mergeTree n   (newLeaf a)

updateVal :: (Monoid a) => RMQ a -> Int -> a -> RMQ a
updateVal Leaf i v  = error "Index not found in RMQ"
updateVal n@Node{..} i v
    | 1 == treeSize && 0 == i   = n { nodeVal = v }
    | i < mid                   = mergeTree (updateVal lhs i v) rhs
    | otherwise                 = mergeTree lhs (updateVal rhs (i - mid) v)
    where mid = rangeSize lhs

elementAt :: (Monoid a) => RMQ a -> Int -> a    -- TODO: could be made without Monoid by just following paths
elementAt rmq i = rangeQuery rmq (i, i+1)

rangeQuery :: (Monoid a) => RMQ a -> (Int, Int) -> a
rangeQuery Leaf _ = mempty
rangeQuery Node{..} (b, e)
     | b <= 0 && e >= treeSize  = nodeVal
     | e <  mid                 = rangeQuery lhs (b, e)
     | b >= mid                 = rangeQuery rhs (b - mid, e - mid)
     | otherwise                = rangeQuery lhs (b, e) <> rangeQuery rhs (b - mid, e - mid)
     where mid = rangeSize lhs


-- | Public instances

--instance (Monoid a) => Monoid (RMQ a) where
--    mempty  = Leaf
--    mappend = undefined -- TODO: not that easy to define (complete left tree first?)


-- | Private

newLeaf :: a -> RMQ a
newLeaf v = Node 1 v Leaf Leaf

mergeTree :: (Monoid a) => RMQ a -> RMQ a -> RMQ a
mergeTree lhs Leaf = lhs
mergeTree Leaf rhs = rhs
mergeTree lhs rhs = Node {
        treeSize = treeSize lhs + treeSize rhs,
        nodeVal  = nodeVal lhs <> nodeVal rhs,
        lhs      = lhs,
        rhs      = rhs
    }

balancedFold :: (RMQ a -> RMQ a -> RMQ a) -> [RMQ a] -> RMQ a
balancedFold merge rmqs = go [head rmqs] (tail rmqs)
    where
        go acc []     = foldr1 (flip merge) acc
        go []  (r:rs) = go [r] rs
        go acc (r:rs)
            | rangeSize (head acc) > rangeSize r = go (r : acc) rs
            | otherwise                          = go (tail acc) (merge (head acc) r : rs)




