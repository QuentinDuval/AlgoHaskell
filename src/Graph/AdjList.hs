{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Graph.AdjList (
    AdjList,
    makeAdjList,
) where


import qualified Data.Map as M
import Graph.Class


-- | Test graph: unweighted and directed

newtype AdjList nodeT
    = AdjList (M.Map nodeT [nodeT])
    deriving (Show, Eq, Ord, Read)

makeAdjList :: (Ord nodeT) => [(nodeT, [nodeT])] -> AdjList nodeT
makeAdjList = AdjList . M.fromListWith (++)

instance EdgeTo nodeT nodeT where
    target = id

instance (Ord nodeT) => ImplicitGraph (AdjList nodeT) nodeT nodeT where
    adjNodes (AdjList m) n = M.findWithDefault [] n m
