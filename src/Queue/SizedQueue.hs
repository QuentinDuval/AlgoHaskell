module Queue.SizedQueue (
  module Class,
  SizedQueue,
  withSize,
) where

import Queue.Class as Class


-- | Generic implementation to add access to size in constant time
-- For implementations that do not maintain the size as a field

data SizedQueue q a
  = SizedQueue {
    wrapped   :: q a,
    queueSize :: Int
  } deriving (Show, Eq, Ord)

instance (IQueue q) => IQueue (SizedQueue q) where
  isNull                  = isNull . wrapped
  top                     = top . wrapped
  pop (SizedQueue q s)    = SizedQueue (pop q) (s - 1)
  push x (SizedQueue q s) = SizedQueue (push x q) (s + 1)

instance (IQueue q) => ISizedQueue (SizedQueue q) where
  getSize (SizedQueue _ s) = s

withSize :: (ISizedQueue q) => q a -> SizedQueue q a
withSize q = SizedQueue q (getSize q)
