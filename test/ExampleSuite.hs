module ExampleSuite where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Example


exampleTests :: TestTree
exampleTests = testGroup "Example"
  [ SC.testProperty "n! == n * (n-1)! (SmallCheck)" $
      \x -> x > 0 SC.==> factorial x == x * factorial (x - 1)

  , QC.testProperty "n! == n * (n-1)! (QuickCheck)" $
      \x -> x > 0 QC.==> factorial x == x * factorial (x - 1)

  , testCase "0! == 1" $
      factorial 0 @?= 1
  ]
