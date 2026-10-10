module test_26_context_other_variable where

--  Ord a entails Eq a, but says nothing about Eq b.

sgOtherVar :: Ord a => a -> b -> Bool
sgOtherVar x y = y == y
