{-# LANGUAGE ConstraintKinds #-}
module Graph.DFS (
    dfsFrom,
) where

import Control.Arrow
import Control.Monad.State
import qualified Data.Set as S
import Graph.Class


-- | Lazy depth first search

-- Idea: Dispatch the constraints toward different algorithm (ord, or eq)
-- This is hardly possible:
-- You can propose different algorithm via classes, with Constraints that differ (ConstraintsKind)
-- But it gets really hard to do some kind of fallback: try hash if you can, or ord otherwise. It requires SFINAE.

type OrdGraph graphT nodeT edgeT = (ImplicitGraph graphT nodeT edgeT, Ord nodeT)

dfsFrom :: (OrdGraph graphT nodeT edgeT) => graphT -> nodeT -> [nodeT]
dfsFrom g s = evalState (dfsImpl g s) S.empty

dfsImpl :: (OrdGraph graphT nodeT edgeT) => graphT -> nodeT -> State (S.Set nodeT) [nodeT]
dfsImpl g s =
    do  modify (S.insert s)
        (s:) <$> recur (adjNodes g s)
    where
        recur []     = return []
        recur (e:es) = do
            let d = target e
            found <- S.member d <$> get
            if found
                then recur es
                else (++) <$> dfsImpl g d <*> recur es

