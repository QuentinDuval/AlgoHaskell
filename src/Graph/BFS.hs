{-# LANGUAGE ConstraintKinds #-}
module Graph.BFS (
    bfsFrom,
    bfsEdgesFrom,
) where

import Control.Arrow
import qualified Data.Set as S
import Queue.Persistent as Q
import Graph.Class


-- | Lazy breath first search

type OrdGraph graphT nodeT edgeT = (ImplicitGraph graphT nodeT edgeT, Ord nodeT)

bfsFrom :: (OrdGraph graphT nodeT edgeT) => graphT -> nodeT -> [nodeT]
bfsFrom g s = s : fmap target (bfsEdgesFrom g s)

bfsEdgesFrom :: (OrdGraph graphT nodeT edgeT) => graphT -> nodeT -> [edgeT]
bfsEdgesFrom g s = bfsImpl g (Q.create $ adjNodes g s) (S.singleton s)

bfsImpl :: (OrdGraph graphT nodeT edgeT) => graphT -> Queue edgeT -> S.Set nodeT -> [edgeT]
bfsImpl g sources visited
    | Q.isNull sources = []
    | otherwise =
        let (edge, rest)  = (Q.top &&& Q.pop) sources
            dest          = target edge
            newVisited    = S.insert dest visited
            nextAdjacents = filter ((`S.notMember` newVisited) . target) (adjNodes g dest)
            toVisitNext   = foldl (flip Q.push) rest nextAdjacents
            recurBfs      = bfsImpl g toVisitNext newVisited
        in if S.member dest visited
            then recurBfs
            else edge : recurBfs

