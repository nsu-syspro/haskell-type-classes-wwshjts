module Task2Suite where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Task2


task2Tests :: TestTree
task2Tests = testGroup "Task2"
  [ SC.testProperty "sumtorial n == n * sumtorial (n-1) (SmallCheck)" $
      \x -> x > 0 SC.==> sumtorial x == x + sumtorial (x - 1)

  , QC.testProperty "sumtorial n == n * sumtorial (n-1) (QuickCheck)" $
      \x -> x > 0 QC.==> sumtorial x == x + sumtorial (x - 1)

  , testCase "sumtorial 0 == 1" $
      sumtorial 0 @?= 1
  ]
