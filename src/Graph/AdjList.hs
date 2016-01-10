{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Graph.AdjList (
    AdjList,
    makeAdjList,
) where

import Control.Arrow
import qualified Data.Map as M
import Graph.Class


-- | Test graph: unweighted and directed

newtype Edge nodeT = Edge { unEdge :: nodeT }
  deriving (Show, Eq, Ord, Read)

newtype AdjList nodeT
  = AdjList (M.Map nodeT [Edge nodeT])
  deriving (Show, Eq, Ord, Read)

makeAdjList :: (Ord nodeT) => [(nodeT, [nodeT])] -> AdjList nodeT
makeAdjList = AdjList . M.fromListWith (++) . map (second (map Edge))

instance EdgeTo (Edge nodeT) nodeT where
  target (Edge e) = e

instance (Ord nodeT) => ImplicitGraph (AdjList nodeT) nodeT (Edge nodeT) where
  adjNodes (AdjList m) n = M.findWithDefault [] n m


-- | Test graph: weighted and directed

newtype WEdge weightT nodeT = WEdge (weightT, nodeT)
  deriving (Show, Eq, Ord, Read)

newtype WeightedAdjList weightT nodeT
  = WeightedAdjList (M.Map nodeT [WEdge weightT nodeT])
  deriving (Show, Eq, Ord, Read)

makeWeightedGraph :: (Ord nodeT) => [(nodeT, [(weightT, nodeT)])]  -> WeightedAdjList weightT nodeT
makeWeightedGraph = WeightedAdjList . M.fromListWith (++) . map (second (map WEdge))

instance EdgeTo (WEdge weightT nodeT) nodeT where
  target (WEdge e) = snd e

instance Weighted (WEdge weightT nodeT) weightT where
  weight (WEdge e) = fst e
