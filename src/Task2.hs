{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task2 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment
import Prelude hiding (foldl, foldr, sum)

-----------------------------------
--
-- Computes sum from 1 to n
--
-- Usage example:
--
-- >>> sumtorial 5
-- 16

sumtorial :: Integer -> Integer
-- Stub implementation for use in actual assignment
-- factorial = error "TODO: define sumtorial"
sumtorial 0 = 1
sumtorial n = n + sumtorial (n - 1)
