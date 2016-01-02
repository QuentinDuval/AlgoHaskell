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
        let (d, rest)     = (Q.top &&& Q.pop) sources
            newVisited    = S.insert d visited
            nextAdjacents = filter (`S.notMember` newVisited) (target <$> adjNodes g d)
            toVisitNext   = foldl (flip Q.push) rest nextAdjacents
        in if S.member d visited
            then bfsImpl g toVisitNext newVisited
            else d : bfsImpl g toVisitNext newVisited

