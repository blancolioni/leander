module test_51_field_given_twice where

--  Construction naming one field twice (#33).

data T51d = T51d { a51d :: Int }

x51d :: T51d
x51d = T51d { a51d = 1, a51d = 2 }
