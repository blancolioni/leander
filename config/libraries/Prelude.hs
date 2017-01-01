class  Eq a  where  
    (==), (/=) :: a -> a -> Bool  
 
        -- Minimal complete definition:  
        --      (==) or (/=)  
    x /= y     =  not (x == y)  
    x == y     =  not (x /= y)
    
id x = x

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
                             