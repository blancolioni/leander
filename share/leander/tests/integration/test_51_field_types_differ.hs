module test_51_field_types_differ where

--  A field shared by constructors at different types (#33).

data T51b = A51b { f51b :: Int } | B51b { f51b :: Char }
