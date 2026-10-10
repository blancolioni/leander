module test_26_shared_variables where

--  The body makes the signature's two type variables one.

sgShared :: a -> b -> a
sgShared x y = y
