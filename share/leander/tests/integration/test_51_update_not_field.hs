module test_51_update_not_field where

--  An update of something that is not a field (#33).

x51i :: Int
x51i = 3 { nope51i = 2 }
