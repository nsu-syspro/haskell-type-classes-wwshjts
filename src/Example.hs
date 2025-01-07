{-# OPTIONS_GHC -Wall #-}

module Example where

factorial :: Integer -> Integer
-- Stub implementation for use in actual assignment
-- factorial = error "TODO: define factorial"
factorial 0 = 1
factorial n = n * factorial (n - 1)
