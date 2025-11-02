module Prelude where

foreign import skit "#eq" #primIntEq :: Int -> Int -> Int
foreign import skit "#eq" #primCharEq :: Char -> Char -> Char

foreign import skit "#add" #primIntAdd :: Int -> Int -> Int

null [] = True
null ((:) x xs) = False

length [] = 0
length ((:) x xs) = #primIntAdd 1 (length xs)
