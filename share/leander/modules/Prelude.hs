module Prelude where

foreign import skit "#eq" #primIntEq :: Int -> Int -> Bool
foreign import skit "#eq" #primCharEq :: Char -> Char -> Bool

foreign import skit "#id" #primIntToChar :: Int -> Char
foreign import skit "#id" #primCharToInt :: Char -> Int

foreign import skit "#le" #primIntLeq :: Int -> Int -> Bool

foreign import skit "#add" #primIntAdd :: Int -> Int -> Int
foreign import skit "#mul" #primIntMul :: Int -> Int -> Int
foreign import skit "#sub" #primIntSub :: Int -> Int -> Int
foreign import skit "#div" #primIntDiv :: Int -> Int -> Int
foreign import skit "#mod" #primIntMod :: Int -> Int -> Int

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
    x /= y = not (x == y)
    x == y = not (x /= y)

class Eq a => Ord a where
    (<), (<=), (>=), (>) :: a -> a -> Bool
    compare :: a -> a -> Ordering
    max, min :: a -> a -> a
         
    x <= y           =  compare x y /= GT  
    x <  y           =  compare x y == LT  
    x >= y           =  compare x y /= LT  
    x >  y           =  compare x y == GT

    compare x y       =  if x == y then EQ
                         else if x <= y then LT
                         else GT
    max x y           =  if x <= y then y else x
    min x y           =  if x <= y then x else y
    
class Show a where
    show :: a -> [Char]

class  Enum a  where  
    succ, pred       :: a -> a  
    toEnum           :: Int -> a  
    fromEnum         :: a -> Int  
    enumFrom         :: a -> [a]             -- [n..]  
    enumFromThen     :: a -> a -> [a]        -- [n,n'..]  
    enumFromTo       :: a -> a -> [a]        -- [n..m]  
    enumFromThenTo   :: a -> a -> a -> [a]   -- [n,n'..m]  
 
        -- Minimal complete definition:  
        --      toEnum, fromEnum  
        --  
        -- NOTE: these default methods only make sense for types  
        --       that map injectively into Int using fromEnum  
        --       and toEnum.  
   succ             =  toEnum . (+1) . fromEnum  
   pred             =  toEnum . (subtract 1) . fromEnum  
   enumFrom x       =  map toEnum [fromEnum x ..]  
   enumFromTo x y   =  map toEnum [fromEnum x .. fromEnum y]  
   enumFromThen x y =  map toEnum [fromEnum x, fromEnum y ..]  
   enumFromThenTo x y z =  
                       map toEnum [fromEnum x, fromEnum y .. fromEnum z]
                        
class Bounded a where
    minBound :: a
    maxBound :: a

class Functor f where
    fmap :: (a -> b) -> f a -> f b

class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

instance Eq Bool where
    True == b = b
    False == b = not b

instance Show Bool where
    show True  = "True"
    show False = "False"

instance Eq Int where
    (==) = #primIntEq

instance Ord Int where
    x <= y = #primIntLeq x y

instance Bounded Int where
  minBound = #minInt
  maxBound = #maxInt

instance Enum Int where
  succ = (+1)
  pred = \x -> x - 1
  toEnum = id
  fromEnum = id
  enumFrom x = x : enumFrom (x + 1)
  enumFromThen x n = x : enumFromThen n (x + n)
  enumFromTo lo hi = if lo > hi
                     then []
                     else lo : enumFromTo (lo + 1) hi
  enumFromThenTo n n' m = if n > m
                          then []
                          else n : enumFromThenTo n' (n' + n' - n) m

instance Show Int where
  show x = if x == 0
           then "0"
           else if x < 0
           then '-' : reverse (showUnsignedInt (0 - x))
           else reverse (showUnsignedInt x)
                
showUnsignedInt :: Int -> [Char]
showUnsignedInt 0 = ""
showUnsignedInt n = let units = mod n 10
                        rest  = div n 10
                    in toEnum (units + 48) : showUnsignedInt rest
                       
instance Eq Char where
  (==) = #primCharEq

instance Ord Char where
  (<=) c c' = fromEnum c <= fromEnum c'

instance Enum Char where
  toEnum = #primIntToChar
  fromEnum = #primCharToInt
  
instance (Eq a) => Eq [a] where
    (==) [] = \ys -> case ys of
                    [] -> True
                    _  -> False
    (==) (x:xs) = \ys -> case ys of
                    [] -> False
                    (y:ys') -> x == y && xs == ys'

data Ordering = LT | EQ | GT deriving (Eq)

otherwise :: Bool
otherwise = True

id :: a -> a
id x = x

const :: a -> b -> a
const x y = x

(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

seq :: a -> b -> a
seq = #primSeq

($) :: (a -> b) -> a -> b
f $ x = f x

($!) :: (a -> b) -> a -> b
f $! x = seq x (f x)

not :: Bool -> Bool
not True = False
not False = True

(&&), (||) :: Bool -> Bool -> Bool
True && b = b
False && _ = False

True || _ = True
False || b = b

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

mod, div :: Int -> Int -> Int
mod = #primIntMod
div = #primIntDiv

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs


take :: Int -> [a] -> [a]
take 0 _ = []
take _ [] = []
take n (x:xs) = x : take (n - 1) xs

filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs) = if p x then x : filter p xs else filter p xs

sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + sum xs

--  return x = [x]
--  (>>=) xs f = concat (map f xs)

concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ concat xss

reverse :: [a] -> [a]
reverse xs = let rev []     acc = acc
                 rev (x:xs) acc = rev xs (x:acc)
             in rev xs []
                
(++) :: [a] -> [a] -> [a]
(++) [] ys = ys
(++) (x:xs) ys = x : (xs ++ ys)

zero :: Int -> Bool
zero 0 = True
zero _ = False

small :: Int -> Bool
small 0 = True
small 1 = True
small 2 = True
small 3 = True
small x = False

subtract :: Int -> Int -> Int
subtract x y = y - x

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

data  Maybe a  =  Nothing | Just a deriving Eq

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

print :: Show a => a -> IO ()
print x = putStrLn (show x)

runIO :: IO a -> a
runIO a = let getf (IO f) = f
              xw' = getf a 1
          in seq (snd xw') (fst xw')
