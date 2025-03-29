{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Task1Suite where

import Test.Tasty
import Test.Tasty.QuickCheck

import Task1 (evaluateIExpr, IExpr, IExpr(..))


task1Tests :: TestTree
task1Tests = testGroup "Task1"
  [ testProperty "evaluateIExpr" $
      withMaxSuccess 1000 $
        \(Blind input) ->
          let res = eval input
              str = strFormula input
          in counterexample ("unexpected result of evaluateIExpr " ++ show str) $
              evaluateIExpr str === Just res

  , testProperty "evaluateIExpr (parsing error)" $
      withMaxSuccess 1000 $
        \(Blind (Unparseable str)) ->
          counterexample ("unexpected result of evaluateIExpr " ++ show str) $
            evaluateIExpr str === Nothing
  ]

type T = IExpr

instance Arbitrary T where
  arbitrary = sized arbitrarySizedT
  shrink (Lit _) = []
  shrink (Add l r) = shrinkPair Add l r
  shrink (Mul l r) = shrinkPair Mul l r

shrinkPair :: (T -> T -> T) -> T -> T -> [T]
shrinkPair op l r =
    -- shrink to subterms
    [l, r] ++
    -- recursively shrink subterms
    [op l' r' | (l', r') <- shrink (l, r)]

arbitrarySizedT :: Int -> Gen T
arbitrarySizedT 0 = Lit <$> chooseInteger (0, 100)
arbitrarySizedT n = do
  op <- elements [Add, Mul]
  l <- arbitrarySizedT (n `div` 2)
  r <- arbitrarySizedT (n `div` 2)
  pure $ op l r

foldT :: (Integer -> a) -> (a -> a -> a) -> (a -> a -> a) -> T -> a
foldT f add mul (Lit x) = f x
foldT f add mul (Add l r) = add (foldT f add mul l) (foldT f add mul r)
foldT f add mul (Mul l r) = mul (foldT f add mul l) (foldT f add mul r)

strFormula :: T -> String
strFormula = foldT show (strOp "+") (strOp "*")
 where
  strOp op l r = unwords [l, r, op]

eval :: T -> Integer
eval = foldT id (+) (*)

newtype Unparseable = Unparseable String
  deriving Show

instance Arbitrary Unparseable where
  arbitrary = do
    t <- arbitrary
    let elems = words $ strFormula t
    poison <- elements (map show [0..9] ++ ["and", "or", "xor"])
    pos <- chooseInt (0, length elems)
    pure $ Unparseable $ unwords (take pos elems ++ [poison] ++ drop pos elems)
