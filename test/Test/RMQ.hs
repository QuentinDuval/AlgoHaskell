module Test.RMQ (
    runRangeQueryTests
) where



import RMQ.RangeQuery
import Test.HUnit
import Utils.Monoids


-- | All tests

runRangeQueryTests :: Test
runRangeQueryTests = TestList
    [ balancedTestQuery, unbalancedTestQuery, updateTestQuery
    , pushBackTest, appendRangeTest ]


-- | Test cases

balancedTestQuery :: Test
balancedTestQuery =
    let q = rangeFromList [Min 1 .. Min 8]
    in TestCase $ do
        assertEqual "Empty range"   UndefMin $ rangeQuery q (0,0)
        assertEqual "Element at"    (Min 1)  $ elementAt  q 0
        assertEqual "Element at"    (Min 2)  $ elementAt  q 1
        assertEqual "Full range"    (Min 1)  $ rangeQuery q (0,8)
        assertEqual "Right range"   (Min 2)  $ rangeQuery q (1,8)
        assertEqual "Left range"    (Min 1)  $ rangeQuery q (0,7)
        assertEqual "Mid range"     (Min 3)  $ rangeQuery q (2,5)
        assertEqual "Small range"   (Min 8)  $ rangeQuery q (7,8)


updateTestQuery :: Test
updateTestQuery =
    let q1 = rangeFromList [Min 1 .. Min 8]
        q2 = updateVal (updateVal q1 0 (Min 8)) 2 (Min 8)
    in TestCase $ do
        assertEqual "Update min 1"  (Min 2)  $ rangeQuery q2 (0,8)
        assertEqual "Update min 2"  (Min 4)  $ rangeQuery q2 (2,5)


unbalancedTestQuery :: Test
unbalancedTestQuery =
    let q1 = rangeFromList [Min 2 .. Min 8]
        q2 = rangeFromList [Min 3 .. Min 8]
        q3 = rangeFromList [Min 4 .. Min 8]
    in TestCase $ do
        assertEqual "7 elements"    (Min 2)  $ rangeQuery q1 (0,8)
        assertEqual "6 elements"    (Min 3)  $ rangeQuery q2 (0,8)
        assertEqual "5 elements"    (Min 4)  $ rangeQuery q3 (0,8)


pushBackTest :: Test
pushBackTest =
    let q1 = rangeFromList [Min 3 .. Min 8]
        q2 = pushBack (pushBack q1 (Min 2)) (Min 1)
    in TestCase $ do
        assertEqual "Before add"    (Min 3)  $ rangeQuery q1 (0,8)
        assertEqual "After add 1"   (Min 1)  $ rangeQuery q2 (0,8)
        assertEqual "After add 2"   (Min 2)  $ rangeQuery q2 (0,7)


appendRangeTest :: Test
appendRangeTest =
    let q1 = rangeFromList [Min 1 .. Min 5]
        q2 = rangeFromList [Min 6 .. Min 8]
        q3 = appendRange q1 q2
    in TestCase $ do
        assertEqual "Append RMQs"   (Min 1)  $ rangeQuery q3 (0,8)
        assertEqual "Left part"     (Min 1)  $ rangeQuery q3 (0,5)
        assertEqual "Right part"    (Min 6)  $ rangeQuery q3 (5,8)
        assertEqual "Repr"  [Min 1 .. Min 8] $ rangeToList q3

