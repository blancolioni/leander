module test_51_update_no_constructor where

--  An update no single constructor has all the fields of (#33).

data T51f = A51f { f51f :: Int } | B51f { g51f :: Int }

x51f :: T51f
x51f = (A51f 1) { f51f = 2, g51f = 3 }
