module Prelude where

null [] = True
null ((:) x xs) = False

length [] = 0
length ((:) x xs) = length xs
