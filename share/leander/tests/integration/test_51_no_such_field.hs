module test_51_no_such_field where

--  Construction with a field the constructor lacks (#33).

data T51c = T51c { a51c :: Int }

x51c :: T51c
x51c = T51c { b51c = 1 }
