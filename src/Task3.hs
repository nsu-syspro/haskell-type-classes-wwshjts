{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

{-# LANGUAGE MultiParamTypeClasses #-}

module Task3 where
import Task1 (Parse, Parse(..))
import Task2

-- | Solves SAT problem for given boolean formula written in Reverse Polish Notation
--
-- Returns whether given formula is satifiable
-- wrapped into 'Maybe' with 'Nothing' indicating parsing failure.
--
-- Only following binary operations are allowed:
-- - and
-- - or
-- - xor
--
-- All other words are considered as variables.
--
-- Usage example:
--
-- >>> solveSAT "x y and"
-- Just True
-- >>> solveSAT "x y xor x y and and"
-- Just False
-- >>> solveSAT "x y xor x y and or"
-- Just True
-- >>> solveSAT "x xor"
-- Nothing
--
solveSAT :: String -> Maybe Bool
solveSAT s =
    let 
        vars = findVars . words $ s
        expr = (parse s :: Maybe (Expr PBool BoolOp))
    in case expr of 
        Just e  ->  case sequence [evaluateBoolExpr (tapPBool v) e | v <- truthTable vars] of
                        Just ans -> Just . or $ ans
                        Nothing  -> Nothing
        Nothing -> Nothing

-- Parsing

data BoolOp = And | Or | Xor
    deriving Show

-- | "Parsable Bool", used to avoid orphan instances
-- and yeah this is ugly
newtype PBool = PBool { unPBool :: Bool }
    deriving Show

instance Parse PBool where
    parse "0" = Just . PBool $ False
    parse "1" = Just . PBool $ True
    parse _   = Nothing

instance Parse BoolOp where
    parse "and" = Just And
    parse "or"  = Just Or
    parse "xor" = Just Xor
    parse _     = Nothing

-- Evaluation of Boolean expression

instance Eval PBool BoolOp where
    evalBinOp And (PBool l) (PBool r) = PBool (l && r)
    evalBinOp Or  (PBool l) (PBool r) = PBool (l || r)
    evalBinOp Xor (PBool l) (PBool r) = PBool (l /= r)

evaluatePBoolExpr :: [(String, PBool)] -> Expr PBool BoolOp -> Maybe PBool
evaluatePBoolExpr = evalExpr

evaluateBoolExpr :: [(String, PBool)] -> Expr PBool BoolOp -> Maybe Bool
evaluateBoolExpr vars e = 
    case evaluatePBoolExpr vars e of
        Just r  -> Just . unPBool $ r
        Nothing -> Nothing
    
-- Helpers

tapPBool :: [(String, Bool)] -> [(String, PBool)]
tapPBool = map (\t -> (fst t, PBool . snd $ t))

findVars :: [String] -> [String]
findVars s = 
    let
        findVars' acc [] = acc
        findVars' acc (w : ws)
            | (w /= "And") && (w /= "Or") && (w /= "Xor") && (w `notElem` acc) = findVars' (w : acc) ws
            | otherwise = findVars' acc ws
    in findVars' [] s 

truthTable :: [String] -> [[(String, Bool)]]
truthTable []           = [[]]
truthTable (var : vars) = [ (var, b) : bs | b <- [False, True], bs <- truthTable vars ]
