module test_26_local_signature where

--  A signature on a local binding is checked too.

sgLocal :: Int -> Int
sgLocal n = let { g :: a -> a; g x = True } in n
