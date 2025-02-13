{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task1 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment
import Prelude hiding (foldl, foldr, product)

-----------------------------------
--
-- Computes factorial of given number
--
--   n! = 1 * 2 * ... * n
--
-- Usage example:
--
-- >>> factorial 5
-- 120

factorial :: Integer -> Integer
-- Stub implementation for use in actual assignment
-- factorial = error "TODO: define factorial"
factorial 0 = 1
factorial n = n * factorial (n - 1)
