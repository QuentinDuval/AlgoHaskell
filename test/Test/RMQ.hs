module Test.RMQ (
    runRangeQueryTests
) where



import RMQ.RangeQuery
import Test.HUnit
import Utils.Monoids


-- | All tests

runRangeQueryTests :: Test
runRangeQueryTests = TestList [ testQuery ]


testQuery :: Test
testQuery =
    let q1 = rangeFromList (fmap Min [1 .. 8])
        q2 = updateVal (updateVal q1 0 (Min 8)) 2 (Min 8)
    in TestCase $ do
        assertEqual "Empty range"   UndefMin $ rangeQuery q1 (0,0)
        assertEqual "Element at"    (Min 1)  $ elementAt  q1 0
        assertEqual "Element at"    (Min 2)  $ elementAt  q1 1
        assertEqual "Full range"    (Min 1)  $ rangeQuery q1 (0,8)
        assertEqual "Right range"   (Min 2)  $ rangeQuery q1 (1,8)
        assertEqual "Left range"    (Min 1)  $ rangeQuery q1 (0,7)
        assertEqual "Mid range"     (Min 3)  $ rangeQuery q1 (2,5)
        assertEqual "Small range"   (Min 8)  $ rangeQuery q1 (7,8)
        assertEqual "Update min 1"  (Min 2)  $ rangeQuery q2 (0,8)
        assertEqual "Update min 2"  (Min 4)  $ rangeQuery q2 (2,5)



