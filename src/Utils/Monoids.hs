module Utils.Monoids where


-- | Minimum value monoid

data Min
  = Min { getMin :: Int }
  | UndefMin
  deriving (Show, Eq, Ord)

instance Monoid Min where
  mempty             = UndefMin
  mappend a UndefMin = a
  mappend UndefMin b = b
  mappend a b        = Min $ min (getMin a) (getMin b)

instance Enum Min where
  toEnum      = Min
  fromEnum    = getMin


-- | Maximum value monoid

data Max
  = Max { getMax :: Int }
  | UndefMax
  deriving (Show, Eq, Ord)

instance Monoid Max where
  mempty             = UndefMax
  mappend a UndefMax = a
  mappend UndefMax b = b
  mappend a b        = Max $ max (getMax a) (getMax b)
