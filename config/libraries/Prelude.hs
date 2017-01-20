systemName = "Leander"

fix f = f (fix f)

-- Map and append  
--  map :: (a -> b) -> [a] -> [b]  
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
    
	max x y = if x <= y then y else x
	min x y = if x <= y then x else y
	
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

showChar         :: Char -> [Char] -> [Char]
showChar         =  (:)

showString       :: [Char] -> [Char] -> [Char]
showString       =  (++)

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
	
foreign import #putchar :: Char -> World# -> World#

putChar :: Char -> IO ()
putChar ch = IO (\w -> (\w' -> ((),w')) (#putchar ch w))

putX = putChar 'X'

putStr s = mapM_ putChar s

putStrLn s = putStr s >> putChar '\n'

join (IO f) g = IO (\w -> case f w of
                               (x,w') -> case g x of
                                          (IO h) -> h w')
                                          
runIO w (IO f) = case f w of
                   (x,w') -> w'
                   
testPutChar1 = join putX (\_ -> putChar 'Y')
testPutChar = putX >> putChar 'Y'

testPutStrLn = doTestPutStrLn "Hello, world"

doTestPutStrLn [] = putChar '\n'
doTestPutStrLn (x:xs) = putChar x >> doTestPutStrLn xs

id x = x

equal x y = x == y

equal4 w x y z = (w == x) && (y == z)

checkEqual = equal4 1 1 2 2 

const x = \ y -> x

f . g = \x -> f (g x)

flip = \f -> \x -> \y -> f y x

fst (a,b) = a

snd (a,b) = b

data Bool = False | True deriving (Eq,Show)

True && x = x
False && x = False

False || x = x
True || x = True

not False = True
not True = False

-- Ordering type  
 
data  Ordering  =  LT | EQ | GT  

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
                             
testEqBool x = x == True

data NewType a = NewType a

runNewType (NewType f) x = NewType (f x)
testNewType x = runNewType (NewType id) x
