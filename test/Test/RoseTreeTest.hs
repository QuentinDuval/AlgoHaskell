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

runRoseTreeTests :: Test
runRoseTreeTests = TestCase $ do

  let z1 = zipper example
  assertEqual "Root" 1 (deref z1)

  let z2 = firstChild z1
  assertEqual "Root.Left" 2 (deref z2)

  let z3 = rightSibling z2
  assertEqual "Root.Midd" 3 (deref z3)

  let z4 = firstChild z3
  assertEqual "Root.Midd.Child" 6 (deref z4)

  let z5 = update z4 (* 2)
  assertEqual "Root.Midd.Child" 12 (deref z5)

  let newExample = unzipper z5
  assertEqual "New example" False (example == newExample)
