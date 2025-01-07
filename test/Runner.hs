import Test.Tasty

import ExampleSuite

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [exampleTests]
