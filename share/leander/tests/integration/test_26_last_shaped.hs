module test_26_last_shaped where

--  The shape of Prelude's old last: one branch returns a list.

sgLast :: [a] -> a
sgLast (x:xs) = if null xs then [x] else sgLast xs
sgLast [] = error "empty"
