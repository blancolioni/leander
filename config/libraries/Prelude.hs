--  id = \ x -> x
id x = x

const x = \ y -> x

o f g x = f (g x)

flip = \f -> \x -> \y -> f y x

data Bool = False | True
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
either f g (Right y) = f y
                             