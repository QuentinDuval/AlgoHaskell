module Queue.DataSeq (
  DataSeqQueue,
  empty,
  create
) where

import qualified Data.Sequence as S
import Queue.Class


-- | Implementation of a FIFO queue based on the data sequence
-- This implementation is merely used to have a reference in the benchmarks

newtype DataSeqQueue a
  = DataSeqQueue { getSequence :: S.Seq a }
  deriving (Show, Eq, Ord)

empty :: DataSeqQueue a
empty = DataSeqQueue S.empty

create :: [a] -> DataSeqQueue a
create = DataSeqQueue . S.fromList

instance IQueue DataSeqQueue where
  isNull = S.null . getSequence
  top    = (`S.index` 0) . getSequence
  pop    = DataSeqQueue . S.drop 1 . getSequence
  push x = DataSeqQueue . (S.|> x) . getSequence

instance ISizedQueue DataSeqQueue where
  getSize = S.length . getSequence
