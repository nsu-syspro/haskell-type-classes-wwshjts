{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
-- The above pragma enables all warnings

module Task1 where
import Data.Char (isDigit)

-- * Expression data type

-- | Representation of integer arithmetic expressions comprising
-- - Literals of type 'a'
-- - Binary operations 'Add' and 'Mul'
data IExpr =
    Lit Integer
  | Add IExpr IExpr
  | Mul IExpr IExpr
  deriving Show
    

-- * Evaluation

-- | Evaluates given 'IExpr'
--
-- Usage example:
--
-- >>> evalIExpr (Lit 2)
-- 2
-- >>> evalIExpr (Add (Lit 2) (Lit 3))
-- 5
-- >>> evalIExpr (Add (Mul (Lit 3) (Lit 2)) (Lit 3))
-- 9
--
evalIExpr :: IExpr -> Integer
evalIExpr (Lit t)   = t
evalIExpr (Add l r) = evalIExpr l + evalIExpr r
evalIExpr (Mul l r) = evalIExpr l * evalIExpr r 

-- * Parsing

-- | Class of parseable types
class Parse a where
  -- | Parses value 'a' from given string
  -- wrapped in 'Maybe' with 'Nothing' indicating failure to parse
  parse :: String -> Maybe a

-- | Parses given expression in Reverse Polish Notation
-- wrapped in 'Maybe' with 'Nothing' indicating failure to parse
--
-- Usage example:
--
-- >>> parse "2" :: Maybe IExpr
-- Just (Lit 2)
-- >>> parse "2 3 +" :: Maybe IExpr
-- Just (Add (Lit 2) (Lit 3))
-- >>> parse "3 2 * 3 +" :: Maybe IExpr
-- Just (Add (Mul (Lit 3) (Lit 2)) (Lit 3))
-- >>> parse "2 +" :: Maybe IExpr
-- Nothing
-- >>> parse "2 3" :: Maybe IExpr
-- Nothing
--
instance Parse IExpr where
  parse str = case sequence $ tokinize str of
                Just tkns -> buildAst [] tkns
                Nothing   -> Nothing 


data Token = LitT Integer | MulT | AddT deriving Show

instance Parse Token where
    parse ch
        | ch == "*"      = Just MulT
        | ch == "+"      = Just AddT
        | all isDigit ch = Just (LitT (read ch))
        | otherwise      = Nothing

tokinize :: String -> [Maybe Token]
tokinize strs = map parse (words strs)

buildAst :: [IExpr] -> [Token] -> Maybe IExpr
buildAst [expr] []                     = Just expr
buildAst exprs (LitT x : tkns)         = buildAst (Lit x : exprs) tkns
buildAst (l : r : exprs) (AddT : tkns) = buildAst (Add l r : exprs) tkns
buildAst (l : r : exprs) (MulT : tkns) = buildAst (Mul l r : exprs) tkns
buildAst _ _                           = Nothing

-- * Evaluation with parsing

-- | Parses given expression in Reverse Polish Notation and evaluates it
--
-- Returns 'Nothing' in case the expression could not be parsed.
--
-- Usage example:
--
-- >>> evaluateIExpr "2"
-- Just 2
-- >>> evaluateIExpr "2 3 +"
-- Just 5
-- >>> evaluateIExpr "3 2 * 3 +"
-- Just 9
-- >>> evaluateIExpr "2 +"
-- Nothing
-- >>> evaluateIExpr "2 3"
-- Nothing
--
evaluateIExpr :: String -> Maybe Integer
evaluateIExpr s = case parse s :: Maybe IExpr of
                    Just expr -> Just $ evalIExpr expr
                    Nothing -> Nothing
