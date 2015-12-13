module Graph.DFS where

import Control.Arrow
import Control.Monad.State
import qualified Data.Set as S
import Graph.Class



-- | Lazy depth first search

dfsFrom :: (ImplicitGraph graphT nodeT edgeT, Ord nodeT) => graphT -> nodeT -> [nodeT]
dfsFrom g s = evalState (dfsImpl g s) S.empty

dfsImpl :: (ImplicitGraph graphT nodeT edgeT, Ord nodeT) => graphT -> nodeT -> State (S.Set nodeT) [nodeT]
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

