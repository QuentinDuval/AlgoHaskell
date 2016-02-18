module List.ArrayList (
  ArrayList,
  newArray,
  pushBack,
  at,
  merge,
) where

import Data.Vector as V


-- | Tree like view of a array list
-- TODO: Somehow, it is like an IntMap?

branching :: Int
branching = 32

data ArrayList a
  = ArrayList { arrayList :: Vector (ArrayList a)
              , arraySize :: Int
              , nodeLevel :: Int }
  | ArrayVals { arrayVals :: Vector a }
  deriving (Show, Eq, Ord, Read)

-- TODO - Implementation with a big branching factor

emptyArray :: ArrayList a
emptyArray = ArrayVals V.empty

newArray ::[a] -> ArrayList a
newArray = undefined -- Create a list of vectors, then balanced fold to the top

pushBack :: ArrayList a -> a -> ArrayList a
pushBack = undefined -- Potentially adds a new layer at the top

at :: ArrayList a -> Int -> a
at (ArrayVals vals) i = vals ! i
at node i             = undefined

merge :: ArrayList a -> ArrayList a -> ArrayList a
merge = undefined   -- Potentially adds a new layer at the top

-- | Useful instances

instance Functor ArrayList where
  fmap = undefined
