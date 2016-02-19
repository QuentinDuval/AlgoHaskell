module List.IndexListClass where


class IIndexList iList where
  empty     :: iList a
  pushFront :: a -> iList a -> iList a
  getTail   :: iList a -> iList a
  getHead   :: iList a -> a
  at        :: iList a -> Int -> a
  updateAt  :: (a -> a) -> iList a -> Int -> iList a

  fromList  :: [a] -> iList a
  fromList = foldr pushFront empty
