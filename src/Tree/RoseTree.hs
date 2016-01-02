{-# LANGUAGE RecordWildCards #-}
module Tree.RoseTree (
    RoseTree(..),
    RoseTreeZipper(current),
    zipper,
    goUp,
    goDown,
    goLeft,
    goRight,
) where



-- | Rose tree implementation

data RoseTree a
    = RoseTree a [RoseTree a]
    deriving (Show, Eq, Ord)


-- | Zipper implementation

data BreadCrum a
    = BreadCrum {
        value_  :: a,
        lefts_  :: [RoseTree a],
        rights_ :: [RoseTree a]
    }

data RoseTreeZipper a
    = Zipper {
        current     :: RoseTree a,
        breadCrums  :: [BreadCrum a]
    }

zipper :: RoseTree a -> RoseTreeZipper a
zipper rt = Zipper rt []

goUp :: RoseTreeZipper a -> RoseTreeZipper a
goUp z@(Zipper _ []) = z
goUp (Zipper rt (BreadCrum{..}:bs)) =
    let childs = lefts_ ++ [rt] ++ rights_
    in Zipper (RoseTree value_ childs) bs

goDown :: RoseTreeZipper a -> Int -> RoseTreeZipper a
goDown z i = undefined

goLeft :: RoseTreeZipper a -> RoseTreeZipper a
goLeft = undefined

goRight :: RoseTreeZipper a -> RoseTreeZipper a
goRight = undefined
