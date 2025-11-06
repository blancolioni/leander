module Prelude where

foreign import skit "#eq" #primIntEq :: Int -> Int -> Int
foreign import skit "#eq" #primCharEq :: Char -> Char -> Char

foreign import skit "#add" #primIntAdd :: Int -> Int -> Int
foreign import skit "#mul" #primIntMul :: Int -> Int -> Int
foreign import skit "#sub" #primIntSub :: Int -> Int -> Int

foreign import skit "#seq" #primSeq :: a -> b -> b
foreign import skit "#trace" #trace :: a -> a

infixr 9  .
infixr 8  ^, ^^, **
infixl 7  *, /, `quot`, `rem`, `div`, `mod`
infixl 6  +, -

-- The (:) operator is built-in syntax, and cannot legally be given
-- a fixity declaration; but its fixity is given by:
--   infixr 5  :

infix  4  ==, /=, <, <=, >=, >
infixr 3  &&
infixr 2  ||
infixl 1  >>, >>=
infixr 1  =<<
infixr 0  $, $!, `seq`

id x = x

const x = \y -> x

null [] = True
null ((:) x xs) = False

length [] = 0
length ((:) x xs) = #primIntAdd 1 (length xs)

(+) = #primIntAdd
(*) = #primIntMul
(-) = #primIntSub

succ x = x + 1

map f [] = []
map f ((:) x xs) = f x : map f xs

take 0 _ = []
take _ [] = []
take n ((:) x xs) = x : take (n - 1) xs

sum [] = 0
sum ((:) x xs) = x + sum xs

zero 0 = True
zero _ = False

small 0 = True
small 1 = True
small 2 = True
small 3 = True
small x = False

seq = #primSeq
