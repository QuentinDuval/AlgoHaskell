{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Graph.Class where



-- | Abstract definition for an edge

class EdgeTo edgeT nodeT | edgeT -> nodeT where
    target  :: edgeT -> nodeT


-- | Implicit graph (may model things like position in a game)

class (EdgeTo edgeT nodeT) => ImplicitGraph graphT nodeT edgeT | graphT -> edgeT
    where
        adjNodes :: graphT -> nodeT -> [edgeT]


-- | Explicit graph (to which you can add edges, and list nodes)

class (ImplicitGraph graphT nodeT edgeT) => ExplicitGraph graphT nodeT edgeT
    where
        nodeList :: graphT -> [nodeT]
        addEdge  :: graphT -> edgeT -> graphT


