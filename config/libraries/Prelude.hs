--  id = \ x -> x
id x = x

const x = \ y -> x

o = \ f -> \ g -> \ x -> f (g x)
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

maybe = \ n -> \f -> \x -> case x of Nothing -> n
                                     Just z  -> f z

data  Either a b  =  Left a | Right b

either = \f -> \g -> \exy -> case exy of Left x -> f x
                                         Right y -> g y
                             