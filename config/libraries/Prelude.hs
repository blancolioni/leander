systemName = "Leander"

foreign import #putchar :: Char -> World# -> World#
foreign import #intEq :: Int -> Int -> Int
foreign import #objGE :: Int -> Int -> Int
foreign import #objGT :: Int -> Int -> Int
foreign import #objLE :: Int -> Int -> Int
foreign import #objLT :: Int -> Int -> Int

foreign import #fail :: a

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
    compare x y  = if x == y then EQ
                   else if x <= y then LT
                   else GT
 
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
    succ             =  toEnum . (+1) . fromEnum  
    pred             =  toEnum . (subtract 1) . fromEnum  
    enumFrom x       =  map toEnum [fromEnum x ..]  
    enumFromTo x y   =  map toEnum [fromEnum x .. fromEnum y]  
    enumFromThen x y =  map toEnum [fromEnum x, fromEnum y ..]  
    enumFromThenTo x y z =  
                        map toEnum [fromEnum x, fromEnum y .. fromEnum z]
               
primBoolToBool 0 = False
primBoolToBool _ = True
			   
instance Eq Int where
    x == y = primBoolToBool (#intEq x y)
	
instance Ord Int where
    x < y = primBoolToBool (#objLT x y)
    x > y = primBoolToBool (#objGT x y)
    x <= y = primBoolToBool (#objLE x y)
    x >= y = primBoolToBool (#objGE x y)
							   
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

data IO a = IO (World# -> (a,World#))

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

data Bool = False | True deriving (Eq,Ord,Show)

True && x = x
False && x = False

False || x = x
True || x = True

not False = True
not True = False

-- Ordering type  
 
data  Ordering  =  LT | EQ | GT deriving (Eq,Show)

null [] = True
null x = False

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

tests = [("print a list of Bool", print [True,False])
        ,("True is less than False", print (True < False))
        ,("1 == 2", print (1 == 2))
        ,("2 == 2", print (2 == 2))
        ,("1 > 2", print (1 > 2))
        ,("2 > 1", print (2 > 1))
        ,("max False True", print (max False True))
        ]
        
selfTest = do
  putStrLn systemName
  mapM_ (\ x -> case x of (label,test) -> putStr label >> putStr ": " >> test) tests
  
runIO w (IO f) = case f w of
                   (x,w') -> w'
                   