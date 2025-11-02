module Prelude where

foreign import skit "#eq" #primIntEq :: Int -> Int -> Int
foreign import skit "#eq" #primCharEq :: Char -> Char -> Char

foreign import skit "#add" #primIntAdd :: Int -> Int -> Int
foreign import skit "#mul" #primIntMul :: Int -> Int -> Int

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

const x _ = x

null [] = True
null ((:) x xs) = False

length [] = 0
length ((:) x xs) = #primIntAdd 1 (length xs)

(+) = #primIntAdd
(*) = #primIntMul

zero 0 = True
zero _ = False

small 0 = True
small 1 = True
small 2 = True
small 3 = True
small x = False
