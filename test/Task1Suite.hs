module Task1Suite where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Task1


task1Tests :: TestTree
task1Tests = testGroup "Task1"
  [ SC.testProperty "n! == n * (n-1)! (SmallCheck)" $
      \x -> x > 0 SC.==> factorial x == x * factorial (x - 1)

  , QC.testProperty "n! == n * (n-1)! (QuickCheck)" $
      \x -> x > 0 QC.==> factorial x == x * factorial (x - 1)

  , testCase "0! == 1" $
      factorial 0 @?= 1
  ]
