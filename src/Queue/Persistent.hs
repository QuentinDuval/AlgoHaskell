{-# LANGUAGE RecordWildCards #-}
module Queue.Persistent (
    Queue,
    empty,
    create
) where

import Queue.Class


-- | FIFO queue implementation
-- Can be used in persistent setting, as it you cannot trigger several times
-- the reverse but by consuming enough entries to amortize it.

data Queue a = Queue {
    front :: [a],
    back  :: [a],
    fSize :: Int,
    bSize :: Int
} deriving (Show, Eq, Ord)


empty :: Queue a
empty = Queue [] [] 0 0

create :: [a] -> Queue a
create xs = empty { front = xs, fSize = length xs }

instance IQueue Queue where

    isNull  = null . front
    top     = head . front

    pop q@Queue{..} = balance $ q { front = tail front, fSize = fSize - 1 }

    push x q@Queue{..} = balance $ q { back = x : back, bSize = bSize + 1 }


-- | Private

balance :: Queue a -> Queue a
balance q@Queue{..}
    | fSize >= bSize = q
    | otherwise      = Queue (front ++ reverse back) [] (fSize + bSize) 0
