{-# LANGUAGE RecordWildCards #-}
module Tree.RoseTree (
    RoseTree(..),
    RoseTreeZipper(current),
    zipper,
    father,
    firstChild,
    leftSibling,
    rightSibling,
) where



-- | Rose tree implementation

data RoseTree a
    = RoseTree a [RoseTree a]
    deriving (Show, Eq, Ord)


-- | Zipper implementation

data BreadCrum a
    = BreadCrum {
        value_  :: a,            -- ^ Value of father
        lefts_  :: [RoseTree a], -- ^ Left siblings in reverse order
        rights_ :: [RoseTree a]  -- ^ Right siblings in correct order
    }

data RoseTreeZipper a
    = Zipper {
        current     :: RoseTree a,
        breadCrums  :: [BreadCrum a]
    }

zipper :: RoseTree a -> RoseTreeZipper a
zipper rt = Zipper rt []

father :: RoseTreeZipper a -> RoseTreeZipper a
father z@(Zipper _ []) = z
father (Zipper rt (BreadCrum{..}:bs)) =
    let childs = reverse lefts_ ++ [rt] ++ rights_
    in Zipper (RoseTree value_ childs) bs

firstChild :: RoseTreeZipper a -> RoseTreeZipper a
firstChild z = undefined

leftSibling :: RoseTreeZipper a -> RoseTreeZipper a
leftSibling (Zipper rt (BreadCrum{..}:bs)) =
    let childs = undefined
    in undefined

rightSibling :: RoseTreeZipper a -> RoseTreeZipper a
rightSibling = undefined
