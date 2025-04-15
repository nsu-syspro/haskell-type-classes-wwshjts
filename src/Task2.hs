{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

{-# LANGUAGE MultiParamTypeClasses #-}

module Task2 where

import Task1 (Parse, Parse(..))
import Data.Char

-- * Expression data type

-- | Generalized representation of expressions comprising
-- - Literals of type 'a'
-- - Variables with arbitrary 'String' names
-- - Binary operations of type 'op'
data Expr a op =
    Lit a
  | Var String
  | BinOp op (Expr a op) (Expr a op)
  deriving Show

-- | Integer binary operations
data IntOp = Add | Mul | Sub
  deriving Show

-- * Parsing

-- | Parses given expression in Reverse Polish Notation
-- wrapped in 'Maybe' with 'Nothing' indicating failure to parse
--
-- Usage example:
--
-- >>> parse "2" :: Maybe (Expr Integer IntOp)
-- Just (Lit 2)
-- >>> parse "2 3 -" :: Maybe (Expr Integer IntOp)
-- Just (BinOp Sub (Lit 2) (Lit 3))
-- >>> parse "3 2 * 3 +" :: Maybe (Expr Integer IntOp)
-- Just (BinOp Add (BinOp Mul (Lit 3) (Lit 2)) (Lit 3))
-- >>> parse "2 +" :: Maybe (Expr Integer IntOp)
-- Nothing
-- >>> parse "2 3" :: Maybe (Expr Integer IntOp)
-- Nothing
--
instance (Parse a, Parse op) => Parse (Expr a op) where
    parse str = stackParser [] (words str)

-- | Used to avoid orphan instance of Parse Integer
newtype PInteger = PInteger { unPInteger :: Integer } 
    deriving Show

instance Parse PInteger where
    parse str
        | all isDigit str = Just . PInteger . read $ str 
        | otherwise       = Nothing

instance Parse IntOp where
    parse "+" = Just Add
    parse "-" = Just Sub
    parse "*" = Just Mul
    parse  _  = Nothing


type Stack a op = [Expr a op]

stackParser :: (Parse a, Parse op) => Stack a op -> [String] -> Maybe (Expr a op)
stackParser [e] []         = Just e
stackParser stack (w : ws) =
    case stackParserStep stack w of
        Just stack' -> stackParser stack' ws
        Nothing     -> Nothing
stackParser _ _  = Nothing


stackParserStep :: (Parse a, Parse op) => Stack a op -> String -> Maybe (Stack a op)
stackParserStep (p : q : stack) word =
    case parse word of
        Just op -> Just (BinOp op q p : stack)
        Nothing -> stackParserLeaf (p : q : stack) word 
stackParserStep stack word = stackParserLeaf stack word

stackParserLeaf :: (Parse a, Parse op) => Stack a op -> String -> Maybe (Stack a op)
stackParserLeaf stack word = 
    case parse word of
        Just a  -> Just (Lit a : stack)
        Nothing -> Just (Var word : stack) 

-- * Evaluation

-- | Class of evaluatable types
class Eval a op where
  -- | Evaluates given binary operation with provided arguments
    evalBinOp :: op -> a -> a -> a

instance Eval Integer IntOp where
    evalBinOp Add = (+)
    evalBinOp Sub = (-)
    evalBinOp Mul = (*)

instance Eval PInteger IntOp where
    evalBinOp op (PInteger l) (PInteger r) = PInteger $ evalBinOp op l r


-- | Evaluates given 'Expr' using given association list of variable values
--
-- Returns 'Nothing' in case appropriate variable value is missing.
--
-- Usage example:
--
-- >>> evalExpr [] (Lit 2 :: Expr Integer IntOp)
-- Just 2
-- >>> evalExpr [("x", 3)] (BinOp Add (Lit 2) (Var "x")) :: Maybe Integer
-- Just 5
-- >>> evalExpr [("x", 3)] (BinOp Add (Lit 2) (Var "y")) :: Maybe Integer
-- Nothing
--
evalExpr :: (Eval a op) => [(String, a)] -> Expr a op -> Maybe a
evalExpr _ (Lit x)           = Just x 
evalExpr vars (Var name)     = vars `get` name
evalExpr vars (BinOp op l r) = 
    case evalExpr vars l of
        Just el -> case evalExpr vars r of
                    Just er -> Just (evalBinOp op el er) 
                    Nothing -> Nothing
        Nothing -> Nothing
                    

-- | Parses given integer expression in Reverse Polish Notation and evaluates it
-- using given association list of variable values
--
-- Returns 'Nothing' in case the expression could not be parsed
-- or in case appropriate variable value is missing.
--
-- Usage example:
--
-- >>> evaluateInteger [] "2"
-- Just 2
-- >>> evaluateInteger [("x", 3)] "2 x -"
-- Just (-1)
-- >>> evaluateInteger [("x", 3)] "2 y -"
-- Nothing
-- >>> evaluateInteger [] "3 2 * 3 +"
-- Just 9
-- >>> evaluateInteger [] "2 +"
-- Nothing
-- >>> evaluateInteger [] "2 3"
-- Nothing
--
evaluateInteger :: [(String, Integer)] -> String -> Maybe Integer
evaluateInteger vars s = case evaluatePInteger (tapPInteger vars) s of
                            Just x  -> Just . unPInteger $ x
                            Nothing -> Nothing
    where
        tapPInteger = map (\t -> (fst t, PInteger . snd $ t))

evaluatePInteger :: [(String, PInteger)] -> String -> Maybe PInteger
evaluatePInteger = evaluate reifyPInteger
        

-- | Parses given expression in Reverse Polish Notation and evaluates it
-- using given association list of variable values
--
-- Returns 'Nothing' in case the expression could not be parsed
-- or in case appropriate variable value is missing.
--
-- The 'Reify' function is required to reconcile generic type
-- of intermediate 'Expr' expression with concrete type using 'a' and 'op'.
--
evaluate :: (Eval a op, Parse a, Parse op) => Reify a op -> [(String, a)] -> String -> Maybe a
evaluate reify m s = case parse s of
  Just e -> evalExpr m (reify e)
  Nothing -> Nothing

-- * Helpers

-- | Helper type for specifying 'Expr' with
-- concrete 'a' and 'op' in generic context
type Reify a op = Expr a op -> Expr a op

-- | Helper for specifying 'Expr' with 'Integer' and 'IntOp' in generic context
reifyInteger :: Reify Integer IntOp
reifyInteger = id

reifyPInteger :: Reify PInteger IntOp
reifyPInteger = id


get :: [(String, a)] -> String -> Maybe a
get [] _               = Nothing
get ((k, v) : kvs) key = if k == key then Just v else get kvs key 
