{-# LANGUAGE RecordWildCards #-}
module RMQ.RangeQuery where


import Data.Monoid


-- First attemps of implementation of RMQ
-- General case accept any kind of function a -> a -> b
-- Should ask for a typeclass for consistency (or function should change?)

data Min
    = Min { intVal :: Int }
    | UndefMin
    deriving (Show, Eq, Ord)

instance Monoid Min where
    mempty             = UndefMin
    mappend a UndefMin = a
    mappend UndefMin b = b
    mappend a b        = Min $ min (intVal a) (intVal b)


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
    in balancedFold mergeTrees leaves

--pushBack :: (Monoid a) => RMQ a -> a -> RMQ a -- May grow the array from the top
--pushBack = undefined

updateVal :: (Monoid a) => RMQ a -> Int -> a -> RMQ a
updateVal rmq i v = undefined

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

mergeTrees :: (Monoid a) => RMQ a -> RMQ a -> RMQ a
mergeTrees lhs rhs
    = Node {
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
        go acc []     = foldl1 merge acc
        go []  (r:rs) = go [r] rs
        go acc (r:rs)
            | rangeSize (head acc) > rangeSize r = go (r : acc) rs
            | otherwise                          = go (tail acc) (merge (head acc) r : rs)




