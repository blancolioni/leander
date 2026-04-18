module Test14_Operators where

(<==>) :: Bool -> Bool -> Bool
a <==> b = (a && b) || (not a && not b)

main = print (True <==> False)

