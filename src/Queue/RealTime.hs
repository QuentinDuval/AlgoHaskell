{-# LANGUAGE RecordWildCards #-}
module Queue.RealTime (
  module Class,
  Queue,
  empty,
  create
) where

import Queue.Class as Class


-- | FIFO queue implementation
-- This implementation works great in persistent settings
-- By using a schedule, it trades amortized bounds to worse-case bounds

-- Invariants:
-- * The back queue cannot be more than 1 bigger than the front queue
-- * No size tacking is needed for when to reverse, we can use the schedule

data Queue a  = Queue {
  front ::    [a],  -- ^ Front of the queue (lazy)
  back  ::    [a],  -- ^ Back of the queue (lazy)
  schedule :: [a]   -- ^ Schedule of the queue (list of lazy tasks to trigger)
} deriving (Show, Eq, Ord)

empty :: Queue a
empty = Queue [] [] []

create :: [a] -> Queue a
create xs = empty { front = xs }

instance IQueue Queue where

    isNull  = null . front
    top     = head . front

    pop q@Queue{..}        = exec $ q { front = tail front }

    push x (Queue [] [] _) = Queue [x] [] []
    push x q@Queue{..}     = exec $ q { back = x : back }


-- | Private

exec :: Queue a -> Queue a
exec q@(Queue _ _ [])     = balance q           -- ^ No tasks left, time to reverse
exec q@(Queue _ _ (s:sc)) = q { schedule = sc } -- ^ Execute first task

balance :: Queue a -> Queue a
balance q@Queue{..} = Queue fs [] fs
  where fs = incrementalReverse front back []

incrementalReverse :: [a] -> [a] -> [a] -> [a]
incrementalReverse fronts [] acc = fronts ++ acc
incrementalReverse [] [back] acc = back : acc
incrementalReverse (f:fronts) (b:backs) acc = f : incrementalReverse fronts backs (b:acc)
