module Queue.Transient (
    Queue,
    empty,
    create
) where

import Queue.Class



-- | FIFO queue implementation
-- This implementation works great when *not* used in persistent settings
--
-- When used in persistent settings:
-- * One can constantly trigger "reverse" by adding when the front list is empty
-- * This would degrade the queue O(1) amortized bound to O(N).

data Queue a = Queue {
    front :: [a],
    back  :: [a]
} deriving (Show, Eq, Ord)


empty :: Queue a
empty = Queue [] []

create :: [a] -> Queue a
create xs = empty { front = xs }

instance IQueue Queue where

    isNull  = null . front
    top     = head . front

    pop (Queue [_] bs) = Queue (reverse bs) []
    pop (Queue fs  bs) = Queue (tail fs) bs

    push x (Queue [] bs)  = Queue (reverse bs) [x]
    push x q@(Queue _ bs) = q { back = x : bs }


instance ISizedQueue Queue where
    getSize q = length (front q) + length (back q)  -- ^ Inefficient, use the SizedQueue wrapper
