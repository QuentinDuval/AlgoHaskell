{-# LANGUAGE RecordWildCards #-}
module Queue.Persistent (
    module Class,
    Queue,
    empty,
    create
) where

import Queue.Class as Class


-- | FIFO queue implementation
-- This implementation works great in persistent settings
--
-- You cannot trigger several times the reverse without first paying
-- for its amortization (by consuming or adding enough entries).
--
-- Invariants:
-- * The back queue cannot be more than 1 bigger than the front queue
-- * We maintain it by keeping the length of front and rear lists

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

    pop q@Queue{..}    = balance $ q { front = tail front, fSize = fSize - 1 }
    push x q@Queue{..} = balance $ q { back = x : back, bSize = bSize + 1 }


-- | Private

balance :: Queue a -> Queue a
balance q@Queue{..}
    | fSize >= bSize = q
    | otherwise      = Queue (front ++ reverse back) [] (fSize + bSize) 0
