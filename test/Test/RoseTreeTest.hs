module Test.RoseTreeTest (
    runRoseTreeTests
) where

import Test.HUnit
import Tree.RoseTree

-- | All tests

example :: RoseTree Int
example = RoseTree 1 [
    RoseTree 2 [RoseTree 4 [], RoseTree 5 []],
    RoseTree 3 [RoseTree 6 [], RoseTree 7 []],
    RoseTree 8 []
  ]

resultTree :: RoseTree Int
resultTree = RoseTree 1 [
    RoseTree 2 [RoseTree 2 [], RoseTree 5 []],
    RoseTree 6 [RoseTree 8 [], RoseTree 7 []],
    RoseTree 8 []
  ]

runRoseTreeTests :: Test
runRoseTreeTests = TestCase $ do

  let z1 = zipper example
  assertEqual "Root" 1 (deref z1)

  let z2 = firstChild (firstChild z1)
  assertEqual "Root.Left.Left" 4 (deref z2)

  let z3 = father (update z2 (`div` 2))
  assertEqual "Root.Left" 2 (deref z3)

  let z4 = update (rightSibling z3) (*2)
  assertEqual "Root.Midd" 6 (deref z4)

  let z5 = update (firstChild z4) (+2)
  assertEqual "Root.Midd.Child" 8 (deref z5)

  assertEqual "Root.Result" resultTree (unzipper z5)
