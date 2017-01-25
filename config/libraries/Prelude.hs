systemName = "Leander"

foreign import #putchar :: Char -> World# -> World#
foreign import #intEq :: Int -> Int -> Int
foreign import #objGE :: Int -> Int -> Int
foreign import #objGT :: Int -> Int -> Int
foreign import #objLE :: Int -> Int -> Int
foreign import #objLT :: Int -> Int -> Int

foreign import #charVal :: Int -> Char
foreign import #charPos :: Char -> Int

foreign import #intFirst :: Int
foreign import #intLast :: Int

foreign import #intPlus :: Int -> Int -> Int
foreign import #intMinus :: Int -> Int -> Int
foreign import #intMult :: Int -> Int -> Int

foreign import #fail :: a
foreign import #undefined :: a
foreign import #error :: [Char] -> a

foreign import #initWorld :: World#

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


fix f = f (fix f)

undefined = #undefined

error = #error

-- Standard types, classes, instances and related functions  
 
-- Equality and Ordered classes  
 
class  Eq a  where  
    (==), (/=) :: a -> a -> Bool  
 
        -- Minimal complete definition:  
        --      (==) or (/=)  
    x /= y     =  not (x == y)  
    x == y     =  not (x /= y)
    
class  (Eq a) => Ord a  where  
    compare              :: a -> a -> Ordering  
    (<), (<=), (>=), (>) :: a -> a -> Bool  
    max, min             :: a -> a -> a  
 
        -- Minimal complete definition:  
        --      (<=) or compare  
        -- Using compare can be more efficient for complex types.  
    compare x y  
         | x == y    =  EQ  
         | x <= y    =  LT  
         | otherwise =  GT  
  
    x <= y           =  compare x y /= GT  
    x <  y           =  compare x y == LT  
    x >= y           =  compare x y /= LT  
    x >  y           =  compare x y == GT
	
-- note that (min x y, max x y) = (x,y) or (y,x)  
    max x y | x <= y = y
            | otherwise = x

    min x y | x <= y = x
            | otherwise = y

-- Enumeration and Bounded classes  
 
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
    succ                 =  toEnum . (+1) . fromEnum  
    pred                 =  toEnum . (subtract 1) . fromEnum  
    enumFrom x           =  map toEnum [fromEnum x ..]  
    enumFromTo x y       =  map toEnum [fromEnum x .. fromEnum y]  
    enumFromThen x y     =  map toEnum [fromEnum x, fromEnum y ..]  
    enumFromThenTo x y z = map toEnum [fromEnum x, fromEnum y .. fromEnum z]        
    
class  Bounded a  where  
    minBound         :: a  
    maxBound         :: a
    
class  Show a  where  
    showsPrec        :: Int -> a -> [Char] -> [Char]  
    show             :: a -> [Char]
    showList         :: [a] -> [Char] -> [Char]
 
        -- Mimimal complete definition:  
        --      show or showsPrec  
    showsPrec _ x s   = show x ++ s  
 
    show x            = showsPrec 0 x ""  
 
    showList []       = showString "[]"  
    showList (x:xs)   = showChar '[' . shows x . showl xs  
    
showl []     = showChar ']'  
showl (x:xs) = showChar ',' . shows x . showl xs
                                             
shows            =  showsPrec 0

showLitChar :: Char -> [Char] -> [Char]
showLitChar = (:)

showChar         :: Char -> [Char] -> [Char]
showChar         =  (:)

showString       :: [Char] -> [Char] -> [Char]
showString       =  (++)

class  Eq a => Num a  where  
    (+), (-), (*)    :: a -> a -> a  
    negate           :: a -> a  
    abs, signum      :: a -> a  
    fromInteger      :: Integer -> a  
 
        -- Minimal complete definition:  
        --      All, except negate or (-)  
    x - y            =  x + negate y  
    negate x         =  0 - x
    
instance Enum Char where
    toEnum = #charVal
    fromEnum = #charPos
    
primBoolToBool 0 = False
primBoolToBool _ = True
			   
instance Eq Int where
    x == y = primBoolToBool (#intEq x y)
	
instance Ord Int where
    x < y = primBoolToBool (#objLT x y)
    x > y = primBoolToBool (#objGT x y)
    x <= y = primBoolToBool (#objLE x y)
    x >= y = primBoolToBool (#objGE x y)
    
instance Bounded Int where
    minBound = #intFirst
    maxBound = #intLast

instance Enum Int where
    toEnum = id
    fromEnum = id
    succ = (+1)
    pred = (subtract 1)
    enumFrom x = enumFromThen x (x + 1)
    enumFromTo x y = enumFromThenTo x (x + 1) y
    enumFromThen x y = enumFromThenTo x y maxBound
    enumFromThenTo x y z | y < x = []
                         | (z - (y - x) + 1) < y = [x]
                         | otherwise = x : enumFromThenTo y (y + (y - x)) z

instance Show Int where
    show = showInt
    
--  showInt :: Int -> [Char]
showInt x | x < 10 = [toEnum (x + 48)]
          | otherwise = "<too big!>"
    
instance Num Int where
    (+) = #intPlus
    (-) = #intMinus
    (*) = #intMult
    abs x | x < 0 = negate x
          | otherwise = x
    signum x = case compare x 0 of
                 LT -> negate 1
                 EQ -> 0
                 GT -> 1
    fromInteger = const 0
    
subtract         =  flip (-)
    
instance  Show Char  where  
    showsPrec p '\'' = showString "'\\''"  
    showsPrec p c    = showChar '\'' . showLitChar c . showChar '\''  
 
    showList cs = showChar '"' . showCharList cs  
                 	   
-- showCharList :: [Char] -> [Char] -> [Char]
showCharList [] = showChar '"'
showCharList (x:xs) = case x of 
                        '"' -> showString "\\\"" . showCharList xs
						_   -> showLitChar x . showCharList xs
						
instance  (Show a) => Show [a]  where  
    showsPrec p      = showList
	
class  Functor f  where  
    fmap              :: (a -> b) -> f a -> f b
    
class  Monad m  where  
    (>>=)  :: m a -> (a -> m b) -> m b  
    (>>)   :: m a -> m b -> m b  
    return :: a -> m a  
 
        -- Minimal complete definition:  
        --      (>>=), return  
    m >> k  =  m >>= \_ -> k  

sequence       =  foldr mcons (return [])  
                    where mcons p q = p >>= \x -> q >>= \y -> return (x:y)

sequence_      =  foldr (>>) (return ())

-- The xxxM functions take list arguments, but lift the function or  
-- list element to a monad type  
mapM f xs        =  sequence (map f xs)

mapM_ f xs       =  sequence_ (map f xs)

f =<< x          =  x >>= f

data IO a = IO { worldFunction :: World# -> (a,World#) }

instance Monad IO where
   return x = IO (\ w -> (x, w))
   (IO f) >>= g = IO (\w -> case f w of
                               (x,w') -> case g x of
                                          (IO h) -> h w')
	

putChar :: Char -> IO ()
putChar ch = IO (\w -> (\w' -> ((),w')) (#putchar ch w))

putX = putChar 'X'

putStr s = mapM_ putChar s

putStrLn s = putStr s >> putChar '\n'

print = putStrLn . show

id x = x

equal x y = x == y

equal4 w x y z = (w == x) && (y == z)

checkEqual = equal4 1 1 2 2 

const x = \ y -> x

f . g = \x -> f (g x)

flip = \f -> \x -> \y -> f y x

fst (a,b) = a

snd (a,b) = b

data Bool = False | True deriving (Eq,Ord,Show,Enum)

True && x = x
False && x = False

False || x = x
True || x = True

not False = True
not True = False

-- Ordering type  
 
data  Ordering  =  LT | EQ | GT deriving (Eq,Show)

-- head             :: [a] -> a  
head (x:_)       =  x  
head []          =  error "Prelude.head: empty list"

--  tail             :: [a] -> [a]  
tail (_:xs)      =  xs  
tail []          =  error "Prelude.tail: empty list"

-- last             :: [a] -> a  
last []          =  error "Prelude.last: empty list"
last (x:xs)      = case xs of []     -> x
                              (y:ys) -> last ys

-- init             :: [a] -> [a]  
init [x]         =  []  
init (x:xs)      =  x : init xs  
init []          =  error "Prelude.init: empty list"

null             :: [a] -> Bool  
null []          =  True  
null (_:_)       =  False

--  length [] = 0
--  length (_:xs) = 1 + length xs

otherwise = True

data Maybe a = Nothing | Just a

maybe n f Nothing = n
maybe n f (Just z) = f z

instance  Functor Maybe  where  
    fmap f Nothing    =  Nothing  
    fmap f (Just x)   =  Just (f x)
	
data  Either a b  =  Left a | Right b

either f g (Left x) = f x
either f g (Right y) = g y
                             
-- Map and append  
map :: (a -> b) -> [a] -> [b]  
map f []     = []  
map f (x:xs) = f x : map f xs

[]     ++ ys = ys  
(x:xs) ++ ys = x : (xs ++ ys)

filter p []                 = []  
filter p (x:xs) = if p x then x : filter p xs else filter p xs

concat xss = foldr (++) [] xss

concatMap f = concat . map f

-- foldl, applied to a binary operator, a starting value (typically the  
-- left-identity of the operator), and a list, reduces the list using  
-- the binary operator, from left to right:  
--  foldl f z [x1, x2, ..., xn] == (...((z ‘f‘ x1) ‘f‘ x2) ‘f‘...) ‘f‘ xn  
-- foldl1 is a variant that has no starting value argument, and  thus must  
-- be applied to non-empty lists.  scanl is similar to foldl, but returns  
-- a list of successive reduced values from the left:  
--      scanl f z [x1, x2, ...] == [z, z ‘f‘ x1, (z ‘f‘ x1) ‘f‘ x2, ...]  
-- Note that  last (scanl f z xs) == foldl f z xs.  
-- scanl1 is similar, again without the starting element:  
--      scanl1 f [x1, x2, ...] == [x1, x1 ‘f‘ x2, ...]  
 
foldl f z []     =  z  
foldl f z (x:xs) =  foldl f (f z x) xs

-- foldr, foldr1, scanr, and scanr1 are the right-to-left duals of the  
-- above functions.  
 
foldr f z []     =  z  
foldr f z (x:xs) =  f x (foldr f z xs)

data TestRecord = Rec { boolField :: Bool, textField :: [Char] }

testRecValue = Rec { boolField = True, textField = "test record" }

testArithSequence = [1 .. 5]
checkParse = True

tests = [("print a list of Bool", print [True,False])
        ,("True is less than False", print (True < False))
        ,("1 == 2", print (1 == 2))
        ,("2 == 2", print (2 == 2))
        ,("1 > 2", print (1 > 2))
        ,("2 > 1", print (2 > 1))
        ,("max False True", print (max False True))
        ,("head [False,True]", print (head [False,True]))
        ,("tail [False,True]", print (tail [False,True]))
        ,("test record 1", print (boolField testRecValue))
        ,("test record 2", putStrLn (textField testRecValue { textField = "updated test record" }))
        ,("show int", print 1)
        ,("list of int", print [1,2,3,4])
        ,("arithmetic sequence", print testArithSequence)
--        ,("error", print (head (tail [True])))
--        ,("toEnum [0,1]", print (True : map toEnum [0,1]))
        ,("alphabet", print ['A' .. 'Z'])
        ]
        

selfTest = do
  putStrLn systemName
  mapM_ (\ x -> case x of (label,test) -> putStr label >> putStr ": " >> test) tests
  
runIO w (IO f) = case f w of
                   (x,w') -> w'
