module Prelude where

foreign import skit "#eq" #primIntEq :: Int -> Int -> Int
foreign import skit "#eq" #primCharEq :: Char -> Char -> Char

foreign import skit "#add" #primIntAdd :: Int -> Int -> Int

null [] = True
null ((:) x xs) = False

length [] = 0
length ((:) x xs) = #primIntAdd 1 (length xs)

zero 0 = True
zero _ = False

small 0 = True
small 1 = True
small 2 = True
small 3 = True
small x = False
