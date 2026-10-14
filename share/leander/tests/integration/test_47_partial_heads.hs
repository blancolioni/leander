module test_47_partial_heads where

--  An instance head may partly apply a type constructor of two or more
--  parameters, including (,) and (->) written as prefix names (#84).
--  The Prelude's instances for Either e, ((,) a) and ((->) r) are written
--  that way; this one is the module's own.  Run with a handle of its
--  own, since its instance would otherwise be seen by every later module.

instance Functor ((,,) a b) where
    fmap f (a, b, x) = (a, b, f x)

right :: Either Char Int
right = Right 2

--  Either as a monad: the first Left stops it.
safeDiv :: Int -> Int -> Either String Int
safeDiv _ 0 = Left "divide by zero"
safeDiv n d = Right (div n d)

noValue :: Either String Int
noValue = Left "no"

divBoth :: Int -> Either String Int
divBoth n = do
  a <- safeDiv 100 n
  b <- safeDiv a 2
  return (a + b)

--  The reader monad: each step is given the same argument.
addBoth :: Int -> Int
addBoth = do
  doubled <- (* 2)
  plusTen <- (+ 10)
  return (doubled + plusTen)

--  The prefix forms in an ordinary signature.
inc :: (->) Int Int
inc = (+ 1)

pair :: (,) Int Char
pair = (1, 'a')

main :: IO ()
main = do
  print (fmap (+ 1) right, fmap (+ 1) (Left 'x'))
  print (fmap length ('x', "four"), fmap negate ('a', True, 5))
  print (addBoth 3, (fmap (+) (* 2) <*> (+ 10)) 3)
  print (inc 1, pair)
  print (divBoth 5, divBoth 0, fmap (+ 1) noValue)
