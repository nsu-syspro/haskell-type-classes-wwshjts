{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Task3Suite where

import Test.Tasty
import Test.Tasty.QuickCheck

import Task3 (solveSAT)

import Data.Map ((!), Map)
import qualified Data.Map as M
import Data.List (nub)


task3Tests :: TestTree
task3Tests = testGroup "Task3"
  [ testProperty "solveSAT" $
      withMaxSuccess 1000 $
        \(Blind input) ->
          let res = satisfy input
              str = strFormula input
          in counterexample ("unexpected result of solveSAT " ++ show str) $
            classify (not res) "unsatisfiable" $
              solveSAT str === Just res

  , testProperty "solveSAT (parsing error)" $
      withMaxSuccess 1000 $
        \(Blind (Unparseable str)) ->
          counterexample ("unexpected result of solveSAT " ++ show str) $
            solveSAT str === Nothing
  ]

-- Quick verification on samples from task description
--
-- >>> satisfy (And (Xor (V "x") (V "y")) (And (V "x") (V "y")))
-- False
-- >>> satisfy (Or (Xor (V "x") (V "y")) (And (V "x") (V "y")))
-- True

satisfy :: T -> Bool
satisfy t = any (formula t) $ enum (vars t)

enum :: Vars -> [Map String Bool]
enum [] = [M.empty]
enum (v : vs) = [M.insert v b m | m <- enum vs, b <- [True, False]]

type Vars = [String]
type Formula = Map String Bool -> Bool

instance Arbitrary T where
  arbitrary = sized arbitrarySizedT
  shrink (V _) = []
  shrink (And l r) = shrinkPair And l r
  shrink (Or l r)  = shrinkPair Or  l r
  shrink (Xor l r) = shrinkPair Xor l r

shrinkPair :: (T -> T -> T) -> T -> T -> [T]
shrinkPair op l r =
    -- shrink to subterms
    [l, r] ++
    -- recursively shrink subterms
    [op l' r' | (l', r') <- shrink (l, r)]

arbitrarySizedT :: Int -> Gen T
arbitrarySizedT 0 = V <$> elements ["x", "y", "z"]
arbitrarySizedT n = do
  op <- elements [And, Or, Xor]
  l <- arbitrarySizedT (n `div` 2)
  r <- arbitrarySizedT (n `div` 2)
  pure $ op l r

data T = V String | And T T | Or T T | Xor T T
  deriving Show

foldT :: (String -> a) -> (a -> a -> a) -> (a -> a -> a) -> (a -> a -> a) -> T -> a
foldT f and or xor (V s) = f s
foldT f and or xor (And l r) = and (foldT f and or xor l) (foldT f and or xor r)
foldT f and or xor (Or  l r) = or  (foldT f and or xor l) (foldT f and or xor r)
foldT f and or xor (Xor l r) = xor (foldT f and or xor l) (foldT f and or xor r)

strFormula :: T -> String
strFormula = foldT id (strOp "and") (strOp "or") (strOp "xor")
 where
  strOp op l r = unwords [l, r, op]

vars :: T -> Vars
vars = nub . foldT (: []) (++) (++) (++)

formula :: T -> Formula
formula = foldT (flip (!)) (\f g m -> f m && g m) (\f g m -> f m || g m) (\f g m -> f m /= g m)

newtype Unparseable = Unparseable String
  deriving Show

instance Arbitrary Unparseable where
  arbitrary = do
    t <- arbitrary
    let elems = words $ strFormula t
    poison <- elements (vars t ++ ["and", "or", "xor"])
    pos <- chooseInt (0, length elems)
    pure $ Unparseable $ unwords (take pos elems ++ [poison] ++ drop pos elems)
