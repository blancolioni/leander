module Prelude where

foreign import skit "#eq" #primIntEq :: Int -> Int -> Bool
foreign import skit "#eq" #primCharEq :: Char -> Char -> Bool

foreign import skit "#add" #primIntAdd :: Int -> Int -> Int
foreign import skit "#mul" #primIntMul :: Int -> Int -> Int
foreign import skit "#sub" #primIntSub :: Int -> Int -> Int

foreign import skit "#seq" #primSeq :: a -> b -> b
foreign import skit "#trace" #trace :: a -> a

foreign import skit "#putChar" #primPutChar :: Int -> Int -> Char -> Int

foreign import skit "#minInt" #minInt :: Int
foreign import skit "#maxInt" #maxInt :: Int

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

class Eq a where
    (==), (/=) :: a -> a -> Bool
    (/=) x y = not (x == y)
    (==) x y = not (x /= y)

class Eq a => Ord a where
    (<), (<=), (>=), (>) :: a -> a -> Bool

class Show a where
    show :: a -> [Char]

class Bounded a where
    minBound :: a
    maxBound :: a

instance Eq Int where
    (==) = #primIntEq
    (/=) = #primIntEq

instance (Eq a) => Eq [a] where
    (==) [] = \ys -> case ys of
                    [] -> True
                    _  -> False
    (==) (x:xs) = \ys -> case ys of
                    [] -> False
                    (y:ys') -> (x == y) && (xs == ys')
    (/=) xs ys = not (xs == ys)

id :: a -> a
id x = x

const :: a -> b -> a
const x y = x

(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) f g = \x -> f (g x)

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

seq :: a -> b -> a
seq = #primSeq

($) :: (a -> b) -> a -> b
($) f x = f x

($!) :: (a -> b) -> a -> b
($!) f x = seq x (f x)

not :: Bool -> Bool
not True = False
not False = True

(&&), (||) :: Bool -> Bool -> Bool
(&&) True b = b
(&&) False _ = False

(||) True _ = True
(||) False b = b

null :: [a] -> Bool
null [] = True
null (x:xs) = False

length :: [a] -> Int
length [] = 0
length (x:xs) = #primIntAdd 1 (length xs)

(+), (-), (*) :: Int -> Int -> Int
(+) = #primIntAdd
(*) = #primIntMul
(-) = #primIntSub

succ :: Int -> Int
succ x = x + 1

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

take :: Int -> [a] -> [a]
take 0 _ = []
take _ [] = []
take n (x:xs) = x : take (n - 1) xs

sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + sum xs

--  return x = [x]
--  (>>=) xs f = concat (map f xs)

concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ concat xss

(++) :: [a] -> [a] -> [a]
(++) [] ys = ys
(++) (x:xs) ys = x : (xs ++ ys)

equals :: Eq a => a -> a -> Bool
equals x y = x == y

zero :: Int -> Bool
zero 0 = True
zero _ = False

small :: Int -> Bool
small 0 = True
small 1 = True
small 2 = True
small 3 = True
small x = False

-- component projections for pairs:
-- (NB: not provided for triples, quadruples, etc.)

fst              :: (a,b) -> a
fst (x,y)        =  x


snd              :: (a,b) -> b
snd (x,y)        =  y

-- curry converts an uncurried function to a curried function;
-- uncurry converts a curried function to a function on pairs.

curry            :: ((a, b) -> c) -> a -> b -> c
curry f x y      =  f (x, y)


uncurry          :: (a -> b -> c) -> ((a, b) -> c)
uncurry f p      =  f (fst p) (snd p)


foldr            :: (a -> b -> b) -> b -> [a] -> b
foldr f z []     =  z
foldr f z (x:xs) =  f x (foldr f z xs)

-- Maybe type

data  Maybe a  =  Nothing | Just a

maybe :: b -> (a -> b) -> Maybe a -> b
maybe n f Nothing  =  n
maybe n f (Just x) =  f x

showMaybe :: Show a => Maybe a -> [Char]
showMaybe Nothing  = "Nothing"
showMaybe (Just x) = "Just " ++ show x

data IO a = IO (Int -> (a,Int))

returnIO x = IO (\ w -> (x, w))
bindIO (IO f) g = IO (\w -> let xw' = f w
                                ioh = g (fst xw')
                                geth (IO h) = h
                            in geth ioh (snd xw'))

return = returnIO
(>>=) = bindIO
(>>) a b = a >>= \x -> b

sequence :: [IO a] -> IO [a]
sequence = let mcons p q = p >>= \x -> q >>= \y -> return (x:y)
           in foldr mcons (return [])

sequence_ :: [IO a] -> IO ()
sequence_ = foldr (>>) (return ())

mapM :: (a -> IO b) -> [a] -> IO [b]
mapM f as  = sequence (map f as)

mapM_ :: (a -> IO b) -> [a] -> IO ()
mapM_ f as = sequence_ (map f as)

putChar :: Char -> IO ()
putChar ch = IO (\w -> ((), #primPutChar w 1 ch))

putStr :: [Char] -> IO ()
putStr = mapM_ putChar

putStrLn :: [Char] -> IO ()
putStrLn str = putStr str >> putChar '\n'

runIO :: IO a -> a
runIO a = let getf (IO f) = f
              xw' = getf a 1
          in seq (snd xw') (fst xw')
