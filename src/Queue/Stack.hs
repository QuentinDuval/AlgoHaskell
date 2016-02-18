module Queue.Stack (
  module Class,
  Stack,
  empty,
  create,
) where

import Queue.Class as Class


-- | Implementation of a LIFO Queue (Stack)
-- Mainly used as a reference inside the benchmarks

newtype Stack a
  = Stack { getStack :: [a] }
  deriving (Show, Eq, Ord)

empty :: Stack a
empty = Stack []

create :: [a] -> Stack a
create = Stack

instance IQueue Stack where
  isNull              = null . getStack
  top                 = head . getStack
  pop (Stack (x:xs))  = Stack xs
  push x (Stack xs)   = Stack (x:xs)

instance ISizedQueue Stack where
  getSize = length . getStack
