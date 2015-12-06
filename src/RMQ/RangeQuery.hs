{-# LANGUAGE RecordWildCards #-}
module RMQ.RangeQuery where


import Data.Monoid


-- First attempt of implementation of RMQ
-- General case accept any kind of function a -> a -> b
-- Should ask for a typeclass for consistency (or function should change?)

data RMQ a
    = Leaf
    | Node {
        treeSize        :: Int,
        nodeVal         :: a,
        lhs, rhs        :: RMQ a }
    deriving (Show, Eq, Ord)

rangeSize :: RMQ a -> Int
rangeSize Leaf = 0
rangeSize rmq  = treeSize rmq

rangeFromList :: (Monoid a) => [a] -> RMQ a
rangeFromList vals =
    let leaves = zipWith newLeaf [0..] vals
    in balancedFold mergeTree leaves

--pushBack :: (Monoid a) => RMQ a -> a -> RMQ a
--pushBack Leaf a = undefined -- newLeaf i a
--pushBack n@Node{..} a
--    | rangeSize lhs > rangeSize rhs = n { rhs = pushBack rhs a }
--    | otherwise = undefined -- insert new node

updateVal :: (Monoid a) => RMQ a -> Int -> a -> RMQ a
updateVal Leaf i v  = error "Index not found in RMQ"
updateVal n@Node{..} i v
    | isLeafVal n   = n { nodeVal = v }
    | i < mid       = mergeTree (updateVal lhs i v) rhs
    | otherwise     = mergeTree lhs (updateVal rhs (i - mid) v)
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


-- | Private

newLeaf :: Int -> a -> RMQ a
newLeaf i v = Node 1 v Leaf Leaf

isLeafVal :: RMQ a -> Bool
isLeafVal rmq = 1 == treeSize rmq

mergeTree :: (Monoid a) => RMQ a -> RMQ a -> RMQ a
mergeTree lhs Leaf = lhs
mergeTree Leaf rhs = rhs
mergeTree lhs rhs = Node {
        treeSize = treeSize lhs + treeSize rhs,
        nodeVal  = nodeVal lhs <> nodeVal rhs,
        lhs = lhs,
        rhs = rhs
    }

balancedFold :: (RMQ a -> RMQ a -> RMQ a) -> [RMQ a] -> RMQ a
balancedFold merge rmqs = go [head rmqs] (tail rmqs)
    where
        go acc []     = foldr1 (flip merge) acc
        go []  (r:rs) = go [r] rs
        go acc (r:rs)
            | rangeSize (head acc) > rangeSize r = go (r : acc) rs
            | otherwise                          = go (tail acc) (merge (head acc) r : rs)




