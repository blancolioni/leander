module Test15_ConcatOp where

(<==>) :: [a] -> [a] -> [a]
[] <==> ys = ys
(x:xs) <==> ys = x : (xs <==> ys)

main = print (length ([1,2,3] <==> [4,5]) == 5)

