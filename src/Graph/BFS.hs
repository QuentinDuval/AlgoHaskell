{-# LANGUAGE ConstraintKinds #-}
module Graph.BFS (
    bfsFrom,
) where


import Control.Arrow
import Control.Monad.State
import qualified Data.Set as S
import Graph.Class


-- | Lazy breath first search

type OrdGraph graphT nodeT edgeT = (ImplicitGraph graphT nodeT edgeT, Ord nodeT)

bfsFrom :: (OrdGraph graphT nodeT edgeT) => graphT -> nodeT -> [nodeT]
bfsFrom g s = evalState (bfsImpl g [s]) S.empty

bfsImpl :: (OrdGraph graphT nodeT edgeT) => graphT -> [nodeT] -> State (S.Set nodeT) [nodeT]
bfsImpl g []      = pure []
bfsImpl g sources =
    do  modify (`S.union` S.fromList sources)
        visited <- get
        let allAdjacents = target <$> concatMap (adjNodes g) sources
        let toVisitNext  = filter (`S.notMember` visited) allAdjacents
        (sources ++) <$> bfsImpl g toVisitNext

