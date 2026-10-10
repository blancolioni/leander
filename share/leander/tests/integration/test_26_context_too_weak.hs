module test_26_context_too_weak where

--  The body needs Eq a, which the signature does not declare.

sgNoEq :: a -> a -> Bool
sgNoEq x y = x == y
