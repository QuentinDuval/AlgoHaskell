{-# LANGUAGE ConstraintKinds #-}
module Graph.BFS (
    bfsFrom,
) where

import qualified Data.Set as S
import Graph.Class


-- | Lazy breath first search

type OrdGraph graphT nodeT edgeT = (ImplicitGraph graphT nodeT edgeT, Ord nodeT)

bfsFrom :: (OrdGraph graphT nodeT edgeT) => graphT -> nodeT -> [nodeT]
bfsFrom g s = bfsImpl g [s] S.empty

bfsImpl :: (OrdGraph graphT nodeT edgeT) => graphT -> [nodeT] -> S.Set nodeT -> [nodeT]
bfsImpl g [] _              = []
bfsImpl g sources visited   =
    let newVisited   = visited `S.union` S.fromList sources
        allAdjacents = target <$> concatMap (adjNodes g) sources
        toVisitNext  = filter (`S.notMember` newVisited) allAdjacents
    in sources ++ bfsImpl g toVisitNext newVisited

