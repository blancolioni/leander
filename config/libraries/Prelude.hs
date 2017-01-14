systemName = "Leander"

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
	
class  Functor f  where  
    fmap              :: (a -> b) -> f a -> f b
    
class  Monad m  where  
    (>>=)  :: m a -> (a -> m b) -> m b  
    (>>)   :: m a -> m b -> m b  
    return :: a -> m a  
 
        -- Minimal complete definition:  
        --      (>>=), return  
    m >> k  =  m >>= \_ -> k  
	
id x = x

equal x y = x == y

equal4 w x y z = (w == x) && (y == z)

checkEqual = equal4 1 1 2 2 

const x = \ y -> x

o f g x = f (g x)

flip = \f -> \x -> \y -> f y x

fst (a,b) = a

snd (a,b) = b

data Bool = False | True

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

--  maybe = \x1 -> \x2 -> \x3 -> 
data  Either a b  =  Left a | Right b

either f g (Left x) = f x
either f g (Right y) = g y
                             