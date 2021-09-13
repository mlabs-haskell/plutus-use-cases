module Test.Data.Ray where

import Prelude

import Test.Tasty
import Test.Tasty.HUnit as HU

import Mlabs.Data.Ray as R

test :: TestTree
test = testGroup "Data.Ray"
  [
    testCase "fromInteger results in same value as using %" $ do
      let expected = 1 R.% 3
      let actual   = R.fromInteger 333333333333333333333333333
      HU.assertEqual "" expected actual
  ]
