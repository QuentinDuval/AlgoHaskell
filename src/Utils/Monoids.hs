module Utils.Monoids where



-- | Minimum value monoid

data Min
    = Min { intVal :: Int }
    | UndefMin
    deriving (Show, Eq, Ord)

instance Monoid Min where
    mempty             = UndefMin
    mappend a UndefMin = a
    mappend UndefMin b = b
    mappend a b        = Min $ min (intVal a) (intVal b)

instance Enum Min where
    toEnum      = Min
    fromEnum    = intVal

