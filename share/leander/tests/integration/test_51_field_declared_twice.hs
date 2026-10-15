module test_51_field_declared_twice where

--  A field declared twice in one constructor (#33).

data T51a = T51a { a51a :: Int, a51a :: Int }
