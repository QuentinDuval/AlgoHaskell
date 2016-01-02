{-# LANGUAGE ConstraintKinds #-}
module Graph.BFS (
    bfsFrom,
) where

import Control.Arrow
import qualified Data.Set as S
import Queue.Persistent as Q
import Graph.Class


-- | Lazy breath first search

type OrdGraph graphT nodeT edgeT = (ImplicitGraph graphT nodeT edgeT, Ord nodeT)

bfsFrom :: (OrdGraph graphT nodeT edgeT) => graphT -> nodeT -> [nodeT]
bfsFrom g s = bfsImpl g (Q.create [s]) S.empty

bfsImpl :: (OrdGraph graphT nodeT edgeT) => graphT -> Queue nodeT -> S.Set nodeT -> [nodeT]
bfsImpl g sources visited
    | Q.isNull sources = []
    | otherwise =
        let (dest, rest)  = (Q.top &&& Q.pop) sources
            newVisited    = S.insert dest visited
            nextAdjacents = filter (`S.notMember` newVisited) (target <$> adjNodes g dest)
            toVisitNext   = foldl (flip Q.push) rest nextAdjacents
            recurBfs      = bfsImpl g toVisitNext newVisited
        in if S.member dest visited
            then recurBfs
            else dest : recurBfs

