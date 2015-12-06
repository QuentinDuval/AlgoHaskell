{-# LANGUAGE RecordWildCards #-}
module RMQ.RangeQuery where


import Data.Monoid


-- First attempt of implementation of RMQ
-- General case accept any kind of function a -> a -> b
-- Should ask for a typeclass for consistency (or function should change?)

data RMQ a
    = Leaf
    | Node {
        curr, mid, next :: Int,
        nodeVal         :: a,
        lhs, rhs        :: RMQ a }
    deriving (Show, Eq, Ord)

rangeSize :: RMQ a -> Int
rangeSize rmq = next rmq - curr rmq

rangeFromList :: (Monoid a) => [a] -> RMQ a
rangeFromList vals =
    let leaves = zipWith newLeaf [0..] vals
    in balancedFold mergeTree leaves

--pushBack :: (Monoid a) => RMQ a -> a -> RMQ a -- May grow the array from the top
--pushBack = undefined

updateVal :: (Monoid a) => RMQ a -> Int -> a -> RMQ a
updateVal Leaf i v  = error "Index not found in RMQ"
updateVal n@Node{..} i v
    | isLeafVal n   = n { nodeVal = v }
    | i < mid       = mergeTree (updateVal lhs i v) rhs
    | otherwise     = mergeTree lhs (updateVal rhs i v)

elementAt :: (Monoid a) => RMQ a -> Int -> a    -- TODO: could be made without Monoid by just following paths
elementAt rmq i = rangeQuery rmq (i, i+1)

rangeQuery :: (Monoid a) => RMQ a -> (Int, Int) -> a
rangeQuery Leaf _ = mempty
rangeQuery Node{..} (b, e)
     | b <= curr && e >= next = nodeVal
     | b <= curr && e <  mid  = rangeQuery lhs (b, e)
     | b >= mid  && e >= next = rangeQuery rhs (b, e)
     | otherwise              = rangeQuery lhs (b, e) <> rangeQuery rhs (b, e)


-- | Private

newLeaf :: Int -> a -> RMQ a
newLeaf i v = Node i i (i+1) v Leaf Leaf

isLeafVal :: RMQ a -> Bool
isLeafVal Leaf      = False
isLeafVal Node{..}  = curr + 1 == next

mergeTree :: (Monoid a) => RMQ a -> RMQ a -> RMQ a
mergeTree lhs Leaf = lhs
mergeTree Leaf rhs = rhs
mergeTree lhs rhs = Node {
        curr = curr lhs,
        mid  = curr rhs,
        next = next rhs,
        nodeVal = nodeVal lhs <> nodeVal rhs,
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




