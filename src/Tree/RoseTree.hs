{-# LANGUAGE RecordWildCards #-}
module Tree.RoseTree (
    RoseTree(..),

    -- ^ Create and update
    RoseTreeZipper(current),
    zipper,
    unzipper,
    deref,
    update,

    -- ^ Moving in the tree
    father,
    firstChild,
    leftSibling,
    rightSibling,
) where


-- | Rose tree implementation

data RoseTree a
    = RoseTree {
        value  :: a,
        childs :: [RoseTree a]
    } deriving (Show, Eq, Ord)


-- | Zipper implementation

data BreadCrum a
    = BreadCrum {
        crumValue_  :: a,            -- ^ Value of father
        lefts_      :: [RoseTree a], -- ^ Left siblings in reverse order
        rights_     :: [RoseTree a]  -- ^ Right siblings in correct order
    }

data RoseTreeZipper a
    = Zipper {
        current     :: RoseTree a,
        breadCrums  :: [BreadCrum a]
    }


-- | Creating and updating

zipper :: RoseTree a -> RoseTreeZipper a
zipper rt = Zipper rt []

deref :: RoseTreeZipper a -> a
deref = value . current

update :: RoseTreeZipper a -> (a -> a) -> RoseTreeZipper a
update z f =
    let old = current z
        new = old { value = f (value old) }
    in z { current = new }

unzipper :: RoseTreeZipper a -> RoseTree a
unzipper z@(Zipper _ []) = current z
unzipper z = unzipper (father z)


-- | Moving into the tree

father :: RoseTreeZipper a -> RoseTreeZipper a
father z@(Zipper _ []) = z
father (Zipper rt (BreadCrum{..}:bs)) =
    let childs = reverse lefts_ ++ [rt] ++ rights_
    in Zipper (RoseTree crumValue_ childs) bs

firstChild :: RoseTreeZipper a -> RoseTreeZipper a
firstChild (Zipper RoseTree{..} bs) =
    let b = BreadCrum value [] (tail childs)
    in Zipper (head childs) (b : bs)

leftSibling :: RoseTreeZipper a -> RoseTreeZipper a
leftSibling (Zipper rt (BreadCrum{..}:bs)) =
    let b = BreadCrum crumValue_ (tail lefts_) (rt : rights_)
    in Zipper (head lefts_) (b : bs)

rightSibling :: RoseTreeZipper a -> RoseTreeZipper a
rightSibling (Zipper rt (BreadCrum{..}:bs)) =
    let b = BreadCrum crumValue_ (rt : lefts_) (tail rights_)
    in Zipper (head rights_) (b : bs)
