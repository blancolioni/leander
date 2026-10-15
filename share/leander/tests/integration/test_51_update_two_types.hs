module test_51_update_two_types where

--  An update whose fields belong to different types (#33).

data T51e = T51e { a51e :: Int }
data U51e = U51e { b51e :: Int }

x51e :: T51e
x51e = (T51e 1) { a51e = 2, b51e = 3 }
