module Test.Queue (
    runQueueTests
) where


import Queue.Class
import qualified Queue.Persistent as Persistent
import qualified Queue.Transient as Transient
import Queue.Utils
import Test.HUnit


-- | All tests

runQueueTests :: Test
runQueueTests = TestList [
        testQueue Transient.create,
        testQueue Persistent.create
    ]


testQueue :: (IQueue q) => ([Int] -> q Int) -> Test
testQueue create =
    let q1 = push 12 $ push 11 $ pop $ create [0..10 :: Int]
        q2 = foldl (flip push) q1 [13 .. 100 :: Int]
        res = consume (:) [] q2
    in TestCase $ assertEqual "Pop order" [100, 99 .. 1] res

