module ArrayList (
  ArrayList,
  newArray,
  pushBack,
  at,
  merge,
) where

import Data.Vector


-- | Tree like view of a array list

data ArrayList a
  = ArrayList { arrayList :: Vector (ArrayList a) }
  | ArrayVals { arrayVals :: Vector a }
  deriving (Show, Eq, Ord, Read)

-- TODO - Implementation with a big branching factor

newArray ::[a] -> ArrayList a
newArray = undefined

pushBack :: ArrayList a -> a -> ArrayList a
pushBack = undefined

at :: ArrayList a -> Int -> a
at = undefined

merge :: ArrayList a -> ArrayList a -> ArrayList a
merge = undefined

-- | Useful instances

instance Functor ArrayList where
  fmap = undefined
