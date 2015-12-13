{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE FunctionalDependencies #-}
--{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE TypeSynonymInstances #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
module Graph.DFS where

import Control.Arrow
import Control.Monad.State
import qualified Data.Set as S
import Graph.Class



-- | Overload in haskell?

class Print a where
    printThat :: a -> String

instance (Show a) => Print a where
    printThat = show

instance (Show a) => Print (a, Int) where -- optional arg
    printThat (x, n) = concat (replicate n (show x))


-- | Lazy depth first search

-- Idea: Dispatch the constraints toward different algorithm (ord, or eq)
-- This is hardly possible:
-- You can propose different algorithm via classes, with Constraints that differ (ConstraintsKind)
-- But it gets really hard to do some kind of fallback: try hash if you can, or ord otherwise. It requires SFINAE.

type OrdGraph graphT nodeT edgeT = (ImplicitGraph graphT nodeT edgeT, Ord nodeT)

--class DFS graphtT nodeT c | nodeT -> c where
--    dfsFrom :: (ImplicitGraph graphT nodeT edgeT, c) => graphT -> nodeT -> [nodeT]
--
--instance DFS graphT nodeT (Ord nodeT) where
--    dfsFrom g s = evalState (dfsImpl g s) S.empty

--instance DFS graphT nodeT (Eq nodeT) where
--    dfsFrom g s = evalState (dfsImpl g s) S.empty

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

