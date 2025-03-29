{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Task2Suite where

import Test.Tasty
import Test.Tasty.QuickCheck

import Task2 (evaluateInteger, Expr, Expr(..), IntOp, IntOp(..))

import Data.Map ((!), Map)
import qualified Data.Map as M
import Data.List (nub)
import Control.Monad (replicateM)

task2Tests :: TestTree
task2Tests = testGroup "Task2"
  [ testProperty "evaluateInteger" $
      withMaxSuccess 1000 $
        \(Blind (Input expr vs)) ->
          let res = formula expr (M.fromList vs)
              str = strFormula expr
          in counterexample ("unexpected result of evaluateInteger " ++ show vs ++ " " ++ show str) $
            classify (null vs) "without vars" $
              evaluateInteger vs str === Just res

  , testProperty "evaluateInteger (parsing error)" $
      withMaxSuccess 1000 $
        \(Blind (Unparseable str)) ->
          counterexample ("unexpected result of evaluateInteger [] " ++ show str) $
            evaluateInteger [] str === Nothing
  ]


type T = Expr Integer IntOp

data Input = Input T [(String, Integer)]

instance Arbitrary Input where
  arbitrary = do
    t <- arbitrary
    let vs = vars t
    vals <- replicateM (length vs) arbitrary
    pure $ Input t (zip vs vals)
  shrink (Input expr vs) = map (`Input` vs) $ shrink expr


instance Arbitrary T where
  arbitrary = sized arbitrarySizedT
  shrink (Lit _) = []
  shrink (Var _) = []
  shrink (BinOp op l r) =
    -- shrink to subterms
    [l, r] ++
    -- recursively shrink subterms
    [BinOp op l' r' | (l', r') <- shrink (l, r)]


arbitrarySizedT :: Int -> Gen T
arbitrarySizedT 0 = oneof
  [ Lit <$> chooseInteger (0, 100)
  , Var <$> elements ["x", "y", "z"]
  ]
arbitrarySizedT n = do
  op <- elements [Add, Mul, Sub]
  l <- arbitrarySizedT (n `div` 2)
  r <- arbitrarySizedT (n `div` 2)
  pure $ BinOp op l r

foldT :: (Integer -> a) -> (String -> a) -> (IntOp -> a -> a -> a) -> T -> a
foldT f v binOp (Lit x) = f x
foldT f v binOp (Var x) = v x
foldT f v binOp (BinOp op l r) = binOp op (foldT f v binOp l) (foldT f v binOp r)

strFormula :: T -> String
strFormula = foldT show id strOp
 where
  strOp op l r = unwords [l, r, o op]
  o Add = "+"
  o Mul = "*"
  o Sub = "-"

vars :: T -> Vars
vars = nub . foldT (const []) (: []) (const (++))

type Vars = [String]
type Formula = Map String Integer -> Integer

formula :: T -> Formula
formula = foldT const (flip (!)) opFormula

opFormula :: IntOp -> Formula -> Formula -> Formula
opFormula op f g m = f m # g m
 where
   (#) = case op of
     Add -> (+)
     Mul -> (*)
     Sub -> (-)


newtype Unparseable = Unparseable String
  deriving Show

instance Arbitrary Unparseable where
  arbitrary = do
    t <- arbitrary
    let elems = words $ strFormula t
    poison <- elements (map show [0..9] ++ ["x", "y", "z"] ++ ["+", "*", "-"])
    pos <- chooseInt (0, length elems)
    pure $ Unparseable $ unwords (take pos elems ++ [poison] ++ drop pos elems)
