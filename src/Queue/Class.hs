module Queue.Class where


-- | FIFO or LIFO queue abstraction

class IQueue q where
  isNull  :: q a -> Bool
  top     :: q a -> a
  pop     :: q a -> q a
  push    :: a -> q a -> q a


-- | Add the possiblity to get the size

class (IQueue q) => ISizedQueue q where
  getSize :: q a -> Int
