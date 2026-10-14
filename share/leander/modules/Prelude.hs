module Prelude
  ( Bool(..), Either(..), IO(..), IOError, Maybe(..), Ordering(..),
    Applicative(..), Bounded(..), Enum(..), Eq(..), Functor(..),
    Monad(..), Num(..), Ord(..), Read(..), Show(..), all, and, any,
    break, catch, concat, const, curry, cycle, div, drop, dropWhile,
    either, elem, error, filter, flip, foldl, foldl', foldl1, foldr,
    fst, head, id, init, ioError, iterate, last, length, lex, lookup,
    map, mapM, mapM_, maximum, maybe, minimum, mod, not, notElem, null,
    or, otherwise, print, product, putChar, putStr, putStrLn, read,
    readParen, reads, repeat, replicate, reverse, runIO, seq, sequence,
    sequence_, showChar, showParen, showString, shows, snd, span,
    subtract, sum, tail, take, takeWhile, uncurry, unzip, unzip3,
    userError, zip, zip3, zipWith, zipWith3, (!!), ($), ($!), (&&),
    (++), (.), (||)
  ) where

type String = [Char]

foreign import skit "#eq" #primIntEq :: Int -> Int -> Bool
foreign import skit "#eq" #primCharEq :: Char -> Char -> Bool

foreign import skit "#id" #primIntToChar :: Int -> Char
-- Until Integer is arbitrary precision (#86), an Integer is an Int.
foreign import skit "#id" #primIntegerToInt :: Integer -> Int
foreign import skit "#id" #primCharToInt :: Char -> Int

foreign import skit "#le" #primIntLeq :: Int -> Int -> Bool

foreign import skit "#add" #primIntAdd :: Int -> Int -> Int
foreign import skit "#mul" #primIntMul :: Int -> Int -> Int
foreign import skit "#sub" #primIntSub :: Int -> Int -> Int
foreign import skit "#div" #primIntDiv :: Int -> Int -> Int
foreign import skit "#mod" #primIntMod :: Int -> Int -> Int

foreign import skit "#seq" #primSeq :: a -> b -> b
foreign import skit "#trace" #trace :: a -> a
foreign import skit "#errorChar" #errorChar :: Char -> Int -> Int
foreign import skit "#errorRaise" #errorRaise :: Int -> a

foreign import skit "#putChar" #primPutChar :: Int -> Int -> Char -> Int

foreign import skit "#minInt" #minInt :: Int
foreign import skit "#maxInt" #maxInt :: Int

infixr 9  .
infixr 8  ^, ^^, **
infixl 9  !!
infixl 7  *, /, `quot`, `rem`, `div`, `mod`
infixl 6  +, -

-- The (:) operator is built-in syntax, and cannot legally be given
-- a fixity declaration; but its fixity is given by:
--   infixr 5  :
-- and the parser applies it.
infixr 5  ++
infix  4  ==, /=, <, <=, >=, >, `elem`, `notElem`
-- Not a Haskell 2010 Prelude class, but base gives (<*>) this fixity.
infixl 4  <*>
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
    
-- Show, as in the Haskell 2010 Report (section 6.3.3 and the
-- PreludeText module of chapter 9).  An instance defines show or
-- showsPrec; showList is what lets a list of Char show as a string.

type ShowS = String -> String

class Show a where
    showsPrec :: Int -> a -> ShowS
    show      :: a -> String
    showList  :: [a] -> ShowS

        -- Minimal complete definition: show or showsPrec
    showsPrec _ x s = show x ++ s
    show x          = showsPrec 0 x ""

    -- Only class methods and exported names here: a module that imports
    -- the Prelude compiles these defaults again, and sees only what the
    -- Prelude exports.
    showList []     = showString "[]"
    showList (x:xs) = showChar '[' . shows x . showl xs
        where showl []     = showChar ']'
              showl (y:ys) = showChar ',' . shows y . showl ys

shows :: Show a => a -> ShowS
shows = showsPrec 0

showChar :: Char -> ShowS
showChar = (:)

showString :: String -> ShowS
showString = (++)

showParen :: Bool -> ShowS -> ShowS
showParen b p = if b then showChar '(' . p . showChar ')' else p

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

-- Numeric classes  
 
class  (Eq a, Show a) => Num a  where  
    (+), (-), (*)    :: a -> a -> a  
    negate           :: a -> a  
    abs, signum      :: a -> a  
    fromInteger      :: Integer -> a  
 
        -- Minimal complete definition:  
        --      All, except negate or (-)  
    x - y            =  x + negate y  
    negate x         =  0 - x 
    
class Functor f where
    fmap :: (a -> b) -> f a -> f b

class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

-- Applicative is Monad's superclass, as in base (the Applicative-Monad
-- Proposal) rather than the Haskell 2010 Report, which has no
-- Applicative.  So a monad's return is its pure unless it says otherwise.
class Applicative m => Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b
    return = pure
    m >> k = m >>= \_ -> k

instance Eq Bool where
    True == b = b
    False == b = not b

instance Show Bool where
    show True  = "True"
    show False = "False"

-- Bool is built in rather than declared here, so it cannot derive these.
instance Ord Bool where
    False <= _ = True
    True  <= b = b

instance Enum Bool where
    fromEnum False = 0
    fromEnum True  = 1
    toEnum 0 = False
    toEnum 1 = True
    toEnum _ = error "Prelude.Enum.Bool.toEnum: bad argument"
    enumFrom x = enumFromTo x True
    enumFromThen x y = enumFromThenTo x y (fromEnum y >= fromEnum x)

instance Bounded Bool where
    minBound = False
    maxBound = True

instance Bounded Int where
  minBound = #minInt
  maxBound = #maxInt

instance Bounded Char where
  minBound = toEnum 0
  maxBound = toEnum 1114111

instance Enum Int where
  succ = (+1)
  pred = \x -> x - 1
  toEnum = id
  fromEnum = id
  enumFrom x = x : enumFrom (x + 1)
  -- Each step is the gap between the first two, which may be downwards.
  enumFromThen n n' = n : enumFromThen n' (n' + n' - n)
  enumFromTo lo hi = if lo > hi
                     then []
                     else lo : enumFromTo (lo + 1) hi
  enumFromThenTo n n' m = if (if n' >= n then n > m else n < m)
                          then []
                          else n : enumFromThenTo n' (n' + n' - n) m

instance Show Int where
  showsPrec d x = showParen (x < 0 && d > 6) (showString (showSignedInt x))

showSignedInt :: Int -> String
showSignedInt x = if x == 0
                  then "0"
                  else if x < 0
                  then '-' : reverse (showUnsignedInt (0 - x))
                  else reverse (showUnsignedInt x)
                
showUnsignedInt :: Int -> [Char]
showUnsignedInt 0 = ""
showUnsignedInt n = toEnum (units + 48) : showUnsignedInt rest
    where units = mod n 10
          rest  = div n 10
                       
instance Eq Char where
  (==) = #primCharEq

instance Ord Char where
  (<=) c c' = fromEnum c <= fromEnum c'

instance Enum Char where
  toEnum = #primIntToChar
  fromEnum = #primCharToInt

instance Show Char where
  showsPrec _ c = if c == '\''
                  then showString "'\\''"
                  else showChar '\'' . showLitChar c . showChar '\''

  showList cs = showChar '"' . showLitString cs . showChar '"'

-- How a character is written inside a literal (Haskell 2010, section
-- 2.6).  Written with guards rather than character patterns, which the
-- compiler does not take yet.

showLitChar :: Char -> ShowS
showLitChar c
  | c > '\DEL'  = showChar '\\' . protectEsc isDecDigit (shows (fromEnum c))
  | c == '\DEL' = showString "\\DEL"
  | c == '\\'   = showString "\\\\"
  | c >= ' '    = showChar c
  | c == '\a'   = showString "\\a"
  | c == '\b'   = showString "\\b"
  | c == '\f'   = showString "\\f"
  | c == '\n'   = showString "\\n"
  | c == '\r'   = showString "\\r"
  | c == '\t'   = showString "\\t"
  | c == '\v'   = showString "\\v"
  | c == '\SO'  = protectEsc (\h -> h == 'H') (showString "\\SO")
  | otherwise   = showString ('\\' : asciiTab !! fromEnum c)

-- Inside a string, a double quote is escaped too.
showLitString :: String -> ShowS
showLitString []     = id
showLitString (c:cs) = if c == '"'
                       then showString "\\\"" . showLitString cs
                       else showLitChar c . showLitString cs

-- A numeric escape followed by a digit, or \SO followed by H, needs \&
-- between them so that it reads back the same.
protectEsc :: (Char -> Bool) -> ShowS -> ShowS
protectEsc p f = f . cont
    where cont s = case s of
                     []    -> s
                     (c:_) -> if p c then "\\&" ++ s else s

isDecDigit :: Char -> Bool
isDecDigit c = c >= '0' && c <= '9'

asciiTab :: [String]
asciiTab = ["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
            "BS",  "HT",  "LF",  "VT",  "FF",  "CR",  "SO",  "SI",
            "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
            "CAN", "EM",  "SUB", "ESC", "FS",  "GS",  "RS",  "US",
            "SP"]
  
instance Functor [] where
    fmap f [] = []
    fmap f (x:xs) = f x : fmap f xs

instance Applicative [] where
    pure x = [x]
    fs <*> xs = concat (map (\f -> map f xs) fs)

instance Monad [] where
    xs >>= f = concat (map f xs)

instance (Show a) => Show [a] where
    showsPrec _ = showList

instance (Eq a) => Eq [a] where
    (==) [] = \ys -> case ys of
                    [] -> True
                    _  -> False
    (==) (x:xs) = \ys -> case ys of
                    [] -> False
                    (y:ys') -> x == y && xs == ys'

data Ordering = LT | EQ | GT deriving (Eq, Ord, Enum, Bounded)

instance Show Ordering where
    show LT = "LT"
    show EQ = "EQ"
    show GT = "GT"

instance Show () where
    show _ = "()"

instance Eq () where
    _ == _ = True

instance Ord () where
    _ <= _ = True

instance Enum () where
    fromEnum _ = 0
    toEnum 0 = ()
    toEnum _ = error "Prelude.Enum.().toEnum: bad argument"
    enumFrom _ = [()]
    enumFromThen _ _ = repeat ()

instance Bounded () where
    minBound = ()
    maxBound = ()

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

seq :: a -> b -> b
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
length (x:xs) = 1 + length xs

 -- List index (subscript) operator, 0-origin  
(!!)                :: [a] -> Int -> a  
[]     !! _         =  error "Prelude.!!: index too large"  
(x:_)  !! 0         =  x  
(_:xs) !! n         =  xs !! (n-1)

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

 -- iterate f x returns an infinite list of repeated applications of f to x:  
-- iterate f x == [x, f x, f (f x), ...]  
iterate          :: (a -> a) -> a -> [a]  
iterate f x      =  x : iterate f (f x)

-- repeat x is an infinite list, with x the value of every element.  
repeat           :: a -> [a]  
repeat x         =  xs where xs = x:xs

-- replicate n x is a list of length n with x the value of every element  
replicate        :: Int -> a -> [a]  
replicate n x    =  take n (repeat x)

-- cycle ties a finite list into a circular one, or equivalently,  
-- the infinite repetition of the original list.  It is the identity  
-- on infinite lists.  
 
cycle            :: [a] -> [a]  
cycle []         =  error "Prelude.cycle: empty list"  
cycle xs         =  xs' where xs' = xs ++ xs'

take :: Int -> [a] -> [a]
take 0 _ = []
take _ [] = []
take n (x:xs) = x : take (n - 1) xs

drop                   :: Int -> [a] -> [a]  
drop 0 xs              =  xs
drop _ []              =  []  
drop n (x:xs)          =  drop (n - 1) xs 

-- takeWhile, applied to a predicate p and a list xs, returns the longest  
-- prefix (possibly empty) of xs of elements that satisfy p.  dropWhile p xs  
-- returns the remaining suffix.  span p xs is equivalent to  
-- (takeWhile p xs, dropWhile p xs), while break p uses the negation of p.  
 
takeWhile               :: (a -> Bool) -> [a] -> [a]  
takeWhile p []          =  []  
takeWhile p (x:xs)      = if p x then x : takeWhile p xs else []  

dropWhile               :: (a -> Bool) -> [a] -> [a]  
dropWhile p []          =  []  
dropWhile p (x:xs)      = if p x then dropWhile p xs else (x:xs)

span, break             :: (a -> Bool) -> [a] -> ([a],[a])  
span p []            = ([],[])  
span p (x:xs) = if p x
                then let ys = span p xs
                     in (x:fst ys,snd ys)
                else ([],xs)

break p                 =  span (not . p)

 -- maximum and minimum return the maximum or minimum value from a list,  
-- which must be non-empty, finite, and of an ordered type.  
maximum, minimum :: (Ord a) => [a] -> a  
maximum []       =  error "Prelude.maximum: empty list"  
maximum xs       =  foldl1 max xs

minimum []       =  error "Prelude.minimum: empty list"  
minimum xs       =  foldl1 min xs 
-- zip takes two lists and returns a list of corresponding pairs.  If one  
-- input list is short, excess elements of the longer list are discarded.  
-- zip3 takes three lists and returns a list of triples.  Zips for larger  
-- tuples are in the List library  
 
zip              :: [a] -> [b] -> [(a,b)]  
zip x y = case x of
            []     -> []
            (x:xs) -> case y of
                        []     -> []
                        (y:ys) -> (x,y) : zip xs ys

zipWith              :: (a->b->c) -> [a]->[b]->[c]  
zipWith z as bs = case as of
            []     -> []
            (a:as) -> case bs of
                        []     -> []
                        (b:bs) -> z a b : zipWith z as bs


zip3             :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3             =  zipWith3 (,,)

zipWith3         :: (a->b->c->d) -> [a]->[b]->[c]->[d]
zipWith3 z as bs cs = case as of
            []     -> []
            (a:as) -> case bs of
                        []     -> []
                        (b:bs) -> case cs of
                                    []     -> []
                                    (c:cs) -> z a b c : zipWith3 z as bs cs

-- unzip transforms a list of pairs into a pair of lists, and unzip3 a
-- list of triples into a triple of lists.  Each list is built lazily: the
-- pattern binding in the where clause matches only when a list is used.

unzip                :: [(a,b)] -> ([a],[b])
unzip []             =  ([], [])
unzip ((a,b):ps)     =  (a:as, b:bs)
                        where (as, bs) = unzip ps

unzip3               :: [(a,b,c)] -> ([a],[b],[c])
unzip3 []            =  ([], [], [])
unzip3 ((a,b,c):ts)  =  (a:as, b:bs, c:cs)
                        where (as, bs, cs) = unzip3 ts

filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs) = if p x then x : filter p xs else filter p xs

sum, product :: [Int] -> Int
sum = foldl' (+) 0
product = foldl' (*) 1

concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ concat xss

reverse :: [a] -> [a]
reverse xs = let rev []     acc = acc
                 rev (x:xs) acc = rev xs (x:acc)
             in rev xs []
                
 -- and returns the conjunction of a Boolean list.  For the result to be  
-- True, the list must be finite; False, however, results from a False  
-- value at a finite index of a finite or infinite list.  or is the  
-- disjunctive dual of and.  
and, or          :: [Bool] -> Bool  
and              =  foldr (&&) True  
or               =  foldr (||) False

-- Applied to a predicate and a list, any determines if any element  
-- of the list satisfies the predicate.  Similarly, for all.  
any, all         :: (a -> Bool) -> [a] -> Bool  
any p            =  or . map p  
all p            =  and . map p

-- elem is the list membership predicate, usually written in infix form,  
-- e.g., x ‘elem‘ xs.  notElem is the negation.  
elem, notElem    :: (Eq a) => a -> [a] -> Bool  
elem x           =  any (== x)  
notElem x        =  all (/= x) 

-- lookup key assocs looks up a key in an association list.  
lookup           :: (Eq a) => a -> [(a,b)] -> Maybe b  
lookup key []    =  Nothing  
lookup key (xy:xys) = if fst xy == key then Just (snd xy) else lookup key xys
    
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

--  The message goes to the machine a character at a time: foldl' forces
--  each #errorChar call before the next, so they run in order, and only
--  then does #errorRaise raise what they built.
error :: [Char] -> a
error s = #errorRaise (foldl' (\count c -> #errorChar c count) 0 s)

head :: [a] -> a
head (x:_) = x
head [] = error "Prelude.head: empty list"

tail :: [a] -> [a]
tail (_:xs) = xs
tail [] = error "Prelude.tail: empty list"

last             :: [a] -> a  
last (x:xs)      =  if null xs then x else last xs 
last []          =  error "Prelude.last: empty list"

init             :: [a] -> [a]  
init (x:xs)      =  if null xs then [] else x : init xs  
init []          =  error "Prelude.init: empty list"


foldr            :: (a -> b -> b) -> b -> [a] -> b
foldr f z []     =  z
foldr f z (x:xs) =  f x (foldr f z xs)

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
 
foldl            :: (a -> b -> a) -> a -> [b] -> a  
foldl f z []     =  z  
foldl f z (x:xs) =  foldl f (f z x) xs

foldl1           :: (a -> a -> a) -> [a] -> a  
foldl1 f (x:xs)  =  foldl f x xs  
foldl1 _ []      =  error "Prelude.foldl1: empty list"

foldl' f z [] = z
foldl' f z (x:xs) = let z' = f z x
                    in seq z' $ foldl' f z' xs
                       
-- Maybe type

data  Maybe a  =  Nothing | Just a deriving Eq

maybe :: b -> (a -> b) -> Maybe a -> b
maybe n f Nothing  =  n
maybe n f (Just x) =  f x

instance (Show a) => Show (Maybe a) where
    showsPrec _ Nothing  = showString "Nothing"
    showsPrec d (Just x) = showParen (d > 10) (showString "Just " . showsPrec 11 x)

instance Functor Maybe where
    fmap f Nothing = Nothing
    fmap f (Just x) = Just (f x)

instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> mx = fmap f mx

instance Monad Maybe where
    Nothing >>= _ = Nothing
    (Just x) >>= f = f x

 -- Either type  
 
data  Either a b  =  Left a | Right b   deriving (Eq)

instance (Show a, Show b) => Show (Either a b) where
    showsPrec d (Left x)  = showParen (d > 10) (showString "Left " . showsPrec 11 x)
    showsPrec d (Right y) = showParen (d > 10) (showString "Right " . showsPrec 11 y)

either               :: (a -> c) -> (b -> c) -> Either a b -> c  
either f g (Left x)  =  f x  
either f g (Right y) =  g y

-- Standard numeric types.  The data declarations for these types cannot  
-- be expressed directly in Haskell since the constructor lists would be  
-- far too large. 

instance Eq Int where
    (==) = #primIntEq

instance Ord Int where
    x <= y = #primIntLeq x y

instance Num Int where
    (+) = #primIntAdd
    (*) = #primIntMul
    (-) = #primIntSub
    negate x = 0 - x
    abs x = if x < 0 then 0 - x else x
    signum x = if x < 0 then 0 - 1 else if x == 0 then 0 else 1
    fromInteger = #primIntegerToInt

mod, div :: Int -> Int -> Int
mod = #primIntMod
div = #primIntDiv

--  An IO action takes the world token and either succeeds, with its
--  result, or fails with an IOError.  Either way it hands on the world
--  token, so the effects that ran before a failure stay in order with
--  whatever handles it (#88).
data IO a = IO (Int -> IOResult a)

data IOResult a = IOSuccess a Int | IOFailure IOError Int

unIO :: IO a -> Int -> IOResult a
unIO (IO f) = f

--  The Report leaves IOError abstract: userError makes one, and show
--  describes it.  Its kinds grow as primitives that can fail arrive.
data IOErrorKind = IOUserError

data IOError = MkIOError IOErrorKind [Char]

instance Eq IOErrorKind where
    IOUserError == IOUserError = True

instance Eq IOError where
    (MkIOError k s) == (MkIOError k' s') = k == k' && s == s'

instance Show IOError where
    show (MkIOError IOUserError s) = "user error (" ++ s ++ ")"

userError :: [Char] -> IOError
userError s = MkIOError IOUserError s

ioError :: IOError -> IO a
ioError e = IO (\w -> IOFailure e w)

--  The Haskell 98 Prelude's catch: it handles the IOErrors an action
--  raises, not error or a failed match, which are bottom.
catch :: IO a -> (IOError -> IO a) -> IO a
catch a h = IO (\w -> case unIO a w of
                         IOFailure e w' -> seq w' (unIO (h e) w')
                         r              -> r)

instance Functor IO where
    fmap f a = do
        x <- a
        return (f x)

-- (<*>) uses Monad IO, whose return is this instance's pure: the two
-- dictionaries refer to each other, and are linked as one recursive group
-- (#130).
instance Applicative IO where
    pure = returnIO
    mf <*> mx = do
        f <- mf
        x <- mx
        return (f x)

instance Monad IO where
    (>>=) = bindIO

returnIO x = IO (\w -> IOSuccess x w)
--  Forcing w' before going on is what makes an action's effects happen
--  when it runs, rather than when runIO forces the last world token at
--  the end: by then, working out a later action may already have failed,
--  and the output before it would be lost (#151).
bindIO a g = IO (\w -> case unIO a w of
                         IOSuccess x w' -> seq w' (unIO (g x) w')
                         IOFailure e w' -> IOFailure e w')

--  return = returnIO
--  (>>=) = bindIO
--  (>>) a b = a >>= \x -> b

mcons :: (Monad m) => m a -> m [a] -> m [a]
mcons p q = p >>= \x -> q >>= \y -> return (x:y)

sequence :: (Monad m) => [m a] -> m [a]
sequence = foldr mcons (return [])

sequence_ :: (Monad m) => [m a] -> m ()
sequence_ = foldr (>>) (return ())

mapM :: (Monad m) => (a -> m b) -> [a] -> m [b]
mapM f as  = sequence (map f as)

mapM_ :: (Monad m) => (a -> m b) -> [a] -> m ()
mapM_ f as = sequence_ (map f as)

putChar :: Char -> IO ()
putChar ch = IO (\w -> IOSuccess () (#primPutChar w 1 ch))

putStr :: [Char] -> IO ()
putStr = mapM_ putChar

putStrLn :: [Char] -> IO ()
putStrLn str = putStr str >> putChar '\n'

print :: Show a => a -> IO ()
print x = putStrLn (show x)

--  Runs an action, forcing the final world token so that every effect
--  happens.  An IOError nothing caught is a runtime error, like error.
runIO :: IO a -> a
runIO a = case unIO a 1 of
            IOSuccess x w -> seq w x
            IOFailure e w -> seq w (error (show e))

-- Tuple instances.  The Report asks for them up to size 15 (section
-- 6.1.4); they stop at 7 here because each one adds to the time it
-- takes to load the Prelude, and the larger ones add the most.  The
-- tuple types and constructors themselves go up to 15.  Written to one
-- pattern: fields compare left to right.

thenCmp :: Ordering -> Ordering -> Ordering
thenCmp EQ o = o
thenCmp o  _ = o

instance (Eq a1, Eq a2) => Eq (a1,a2) where
    (x1,x2) == (y1,y2) = x1 == y1 && x2 == y2

instance (Ord a1, Ord a2) => Ord (a1,a2) where
    compare (x1,x2) (y1,y2) = thenCmp (compare x1 y1) (compare x2 y2)

instance (Show a1, Show a2) => Show (a1,a2) where
    show (x1,x2) = "(" ++ show x1 ++ "," ++ show x2 ++ ")"

instance (Eq a1, Eq a2, Eq a3) => Eq (a1,a2,a3) where
    (x1,x2,x3) == (y1,y2,y3) = x1 == y1 && x2 == y2 && x3 == y3

instance (Ord a1, Ord a2, Ord a3) => Ord (a1,a2,a3) where
    compare (x1,x2,x3) (y1,y2,y3) = thenCmp (compare x1 y1) (thenCmp (compare x2 y2) (compare x3 y3))

instance (Show a1, Show a2, Show a3) => Show (a1,a2,a3) where
    show (x1,x2,x3) = "(" ++ show x1 ++ "," ++ show x2 ++ "," ++ show x3 ++ ")"

instance (Eq a1, Eq a2, Eq a3, Eq a4) => Eq (a1,a2,a3,a4) where
    (x1,x2,x3,x4) == (y1,y2,y3,y4) = x1 == y1 && x2 == y2 && x3 == y3 && x4 == y4

instance (Ord a1, Ord a2, Ord a3, Ord a4) => Ord (a1,a2,a3,a4) where
    compare (x1,x2,x3,x4) (y1,y2,y3,y4) = thenCmp (compare x1 y1) (thenCmp (compare x2 y2) (thenCmp (compare x3 y3) (compare x4 y4)))

instance (Show a1, Show a2, Show a3, Show a4) => Show (a1,a2,a3,a4) where
    show (x1,x2,x3,x4) = "(" ++ show x1 ++ "," ++ show x2 ++ "," ++ show x3 ++ "," ++ show x4 ++ ")"

instance (Eq a1, Eq a2, Eq a3, Eq a4, Eq a5) => Eq (a1,a2,a3,a4,a5) where
    (x1,x2,x3,x4,x5) == (y1,y2,y3,y4,y5) = x1 == y1 && x2 == y2 && x3 == y3 && x4 == y4 && x5 == y5

instance (Ord a1, Ord a2, Ord a3, Ord a4, Ord a5) => Ord (a1,a2,a3,a4,a5) where
    compare (x1,x2,x3,x4,x5) (y1,y2,y3,y4,y5) = thenCmp (compare x1 y1) (thenCmp (compare x2 y2) (thenCmp (compare x3 y3) (thenCmp (compare x4 y4) (compare x5 y5))))

instance (Show a1, Show a2, Show a3, Show a4, Show a5) => Show (a1,a2,a3,a4,a5) where
    show (x1,x2,x3,x4,x5) = "(" ++ show x1 ++ "," ++ show x2 ++ "," ++ show x3 ++ "," ++ show x4 ++ "," ++ show x5 ++ ")"

instance (Eq a1, Eq a2, Eq a3, Eq a4, Eq a5, Eq a6) => Eq (a1,a2,a3,a4,a5,a6) where
    (x1,x2,x3,x4,x5,x6) == (y1,y2,y3,y4,y5,y6) = x1 == y1 && x2 == y2 && x3 == y3 && x4 == y4 && x5 == y5 && x6 == y6

instance (Ord a1, Ord a2, Ord a3, Ord a4, Ord a5, Ord a6) => Ord (a1,a2,a3,a4,a5,a6) where
    compare (x1,x2,x3,x4,x5,x6) (y1,y2,y3,y4,y5,y6) = thenCmp (compare x1 y1) (thenCmp (compare x2 y2) (thenCmp (compare x3 y3) (thenCmp (compare x4 y4) (thenCmp (compare x5 y5) (compare x6 y6)))))

instance (Show a1, Show a2, Show a3, Show a4, Show a5, Show a6) => Show (a1,a2,a3,a4,a5,a6) where
    show (x1,x2,x3,x4,x5,x6) = "(" ++ show x1 ++ "," ++ show x2 ++ "," ++ show x3 ++ "," ++ show x4 ++ "," ++ show x5 ++ "," ++ show x6 ++ ")"

instance (Eq a1, Eq a2, Eq a3, Eq a4, Eq a5, Eq a6, Eq a7) => Eq (a1,a2,a3,a4,a5,a6,a7) where
    (x1,x2,x3,x4,x5,x6,x7) == (y1,y2,y3,y4,y5,y6,y7) = x1 == y1 && x2 == y2 && x3 == y3 && x4 == y4 && x5 == y5 && x6 == y6 && x7 == y7

instance (Ord a1, Ord a2, Ord a3, Ord a4, Ord a5, Ord a6, Ord a7) => Ord (a1,a2,a3,a4,a5,a6,a7) where
    compare (x1,x2,x3,x4,x5,x6,x7) (y1,y2,y3,y4,y5,y6,y7) = thenCmp (compare x1 y1) (thenCmp (compare x2 y2) (thenCmp (compare x3 y3) (thenCmp (compare x4 y4) (thenCmp (compare x5 y5) (thenCmp (compare x6 y6) (compare x7 y7))))))

instance (Show a1, Show a2, Show a3, Show a4, Show a5, Show a6, Show a7) => Show (a1,a2,a3,a4,a5,a6,a7) where
    show (x1,x2,x3,x4,x5,x6,x7) = "(" ++ show x1 ++ "," ++ show x2 ++ "," ++ show x3 ++ "," ++ show x4 ++ "," ++ show x5 ++ "," ++ show x6 ++ "," ++ show x7 ++ ")"

-- Read, as in the Haskell 2010 Report (section 6.3.3 and the PreludeText
-- module of chapter 9).  The compiler takes neither list comprehensions
-- nor character and string patterns yet, so a comprehension is written
-- as a do block in the list monad, and a literal pattern as a test.

type ReadS a = String -> [(a, String)]

class Read a where
    readsPrec :: Int -> ReadS a
    readList  :: ReadS [a]

        -- Minimal complete definition: readsPrec

    -- Only class methods and exported names here, as for Show.
    readList = readParen False (\r -> do (tok, s) <- lex r
                                         if tok == "[" then readl s else [])
        where readl s  = (do (tok, t) <- lex s
                             if tok == "]" then [([], t)] else [])
                         ++ (do (x, t) <- reads s
                                (xs, u) <- readl' t
                                return (x : xs, u))
              readl' s = do (tok, t) <- lex s
                            if tok == "]"
                            then [([], t)]
                            else if tok == ","
                            then do (x, u) <- reads t
                                    (xs, v) <- readl' u
                                    return (x : xs, v)
                            else []

reads :: Read a => ReadS a
reads = readsPrec 0

read :: Read a => String -> a
read s = case (do (x, t) <- reads s
                  (tok, rest) <- lex t
                  if null tok && null rest then [x] else []) of
           [x] -> x
           []  -> error "Prelude.read: no parse"
           _   -> error "Prelude.read: ambiguous parse"

readParen :: Bool -> ReadS a -> ReadS a
readParen b g = if b then mandatory else optional
    where optional r  = g r ++ mandatory r
          mandatory r = do s <- readToken "(" r
                           (x, t) <- optional s
                           u <- readToken ")" t
                           return (x, u)

-- What follows tok, when the next lexeme of r is tok.
readToken :: String -> String -> [String]
readToken tok r = do (t, s) <- lex r
                     if t == tok then [s] else []

-- The next lexeme of a string, and the rest of it: Haskell's own lexical
-- syntax, without nested comments.
lex :: ReadS String
lex [] = [([], [])]
lex (c:s)
  | isSpace c     = lex (dropWhile isSpace s)
  | c == '\''     = do (ch, t) <- lexLitChar s
                       if ch /= "'" && startsWith "'" t
                       then [('\'' : ch ++ "'", drop 1 t)]
                       else []
  | c == '"'      = do (str, t) <- lexString s
                       return ('"' : str, t)
  | isSingle c    = [([c], s)]
  | isSymbol c    = [(c : takeWhile isSymbol s, dropWhile isSymbol s)]
  | isAlpha c     = [(c : takeWhile isIdChar s, dropWhile isIdChar s)]
  | isDecDigit c  = do (fe, t) <- lexFracExp (dropWhile isDecDigit s)
                       return (c : takeWhile isDecDigit s ++ fe, t)
  | otherwise     = []
    where isSingle x = x `elem` ",;()[]{}_`"
          isSymbol x = x `elem` "!@#$%&*+./<=>?\\^|:-~"
          isIdChar x = isAlphaNum x || x `elem` "_'"

-- The fraction and exponent of a number, when it has them.
lexFracExp :: ReadS String
lexFracExp s = case s of
                 (p:d:t) -> if p == '.' && isDecDigit d
                            then do (e, u) <- lexExp (dropWhile isDecDigit t)
                                    return ('.' : d : takeWhile isDecDigit t ++ e, u)
                            else lexExp s
                 _       -> lexExp s

lexExp :: ReadS String
lexExp s = case s of
             (e:t) -> if e == 'e' || e == 'E'
                      then case t of
                             (sign:d:u) | (sign == '+' || sign == '-') && isDecDigit d ->
                                 [(e : sign : d : takeWhile isDecDigit u, dropWhile isDecDigit u)]
                             (d:u) | isDecDigit d ->
                                 [(e : d : takeWhile isDecDigit u, dropWhile isDecDigit u)]
                             _ -> [([], s)]
                      else [([], s)]
             _     -> [([], s)]

-- The rest of a string literal after its opening quote, closing quote
-- included.  A gap of white space between backslashes becomes \&.
lexString :: ReadS String
lexString [] = []
lexString (c:s)
  | c == '"'  = [("\"", s)]
  | otherwise = do (ch, t) <- lexStringItem (c:s)
                   (str, u) <- lexString t
                   return (ch ++ str, u)

lexStringItem :: ReadS String
lexStringItem s = case s of
                    (b:c:t) -> if b == '\\' && c == '&'
                               then [("\\&", t)]
                               else if b == '\\' && isSpace c
                               then case dropWhile isSpace t of
                                      (e:u) -> if e == '\\' then [("\\&", u)] else []
                                      []    -> []
                               else lexLitChar s
                    _       -> lexLitChar s

-- One character of a literal, as written: itself, or its escape.
lexLitChar :: ReadS String
lexLitChar [] = []
lexLitChar (c:s)
  | c == '\\' = do (esc, t) <- lexEscape s
                   return ('\\' : esc, t)
  | otherwise = [([c], s)]

lexEscape :: ReadS String
lexEscape [] = []
lexEscape (c:s)
  | c `elem` "abfnrtv\\\"'" = [([c], s)]
  | c == '^'                = case s of
                                (d:t) -> if d >= '@' && d <= '_' then [(['^', d], t)] else []
                                []    -> []
  | isDecDigit c            = [(c : takeWhile isDecDigit s, dropWhile isDecDigit s)]
  | otherwise               = case asciiEscape (c:s) of
                                Just (name, t) -> [(name, t)]
                                Nothing        -> []

-- One character of a literal, read: what it stands for.
readLitChar :: ReadS Char
readLitChar [] = []
readLitChar (c:s)
  | c == '\\' = readEscape s
  | otherwise = [(c, s)]

readEscape :: ReadS Char
readEscape [] = []
readEscape (c:s)
  | c == 'a'     = [('\a', s)]
  | c == 'b'     = [('\b', s)]
  | c == 'f'     = [('\f', s)]
  | c == 'n'     = [('\n', s)]
  | c == 'r'     = [('\r', s)]
  | c == 't'     = [('\t', s)]
  | c == 'v'     = [('\v', s)]
  | c == '\\'    = [('\\', s)]
  | c == '"'     = [('"', s)]
  | c == '\''    = [('\'', s)]
  | c == '^'     = case s of
                     (d:t) -> if d >= '@' && d <= '_'
                              then [(toEnum (fromEnum d - 64), t)]
                              else []
                     []    -> []
  | isDecDigit c = [(toEnum (readDecimal (c : takeWhile isDecDigit s)),
                     dropWhile isDecDigit s)]
  | otherwise    = case asciiEscape (c:s) of
                     Just (name, t) -> [(toEnum (asciiCode name), t)]
                     Nothing        -> []

-- The name of a control character at the start of s, as \SOH or \DEL
-- write it, and what follows it.  SOH comes before SO in asciiTab, so
-- the longer name is tried first.
asciiEscape :: String -> Maybe (String, String)
asciiEscape s = case filter (\name -> startsWith name s) ("DEL" : asciiTab) of
                  (name:_) -> Just (name, drop (length name) s)
                  []       -> Nothing

asciiCode :: String -> Int
asciiCode name = if name == "DEL" then 127 else asciiIndex 0 asciiTab
    where asciiIndex n (a:as) = if a == name then n else asciiIndex (n + 1) as
          asciiIndex n []     = 0

readDecimal :: String -> Int
readDecimal = foldl (\n d -> n * 10 + fromEnum d - 48) 0

startsWith :: String -> String -> Bool
startsWith [] _          = True
startsWith (p:ps) (c:cs) = p == c && startsWith ps cs
startsWith _ []          = False

isSpace :: Char -> Bool
isSpace c = c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f'
            || c == '\v' || c == '\xa0'

isAlpha :: Char -> Bool
isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

isAlphaNum :: Char -> Bool
isAlphaNum c = isAlpha c || isDecDigit c

-- A number with an optional minus sign in front, as reads finds it.
readSigned :: ReadS Int -> ReadS Int
readSigned readPos = readParen False read'
    where read' r  = read'' r
                     ++ (do s <- readToken "-" r
                            (x, t) <- read'' s
                            return (negate x, t))
          read'' r = do (str, s) <- lex r
                        (n, rest) <- readPos str
                        if null rest then [(n, s)] else []

readDec :: ReadS Int
readDec s = if null ds then [] else [(readDecimal ds, dropWhile isDecDigit s)]
    where ds = takeWhile isDecDigit s

instance Read Int where
    readsPrec _ = readSigned readDec

instance Read Char where
    readsPrec _ = readParen False
                    (\r -> do (tok, t) <- lex r
                              case tok of
                                (q:s) -> if q == '\''
                                         then do (c, rest) <- readLitChar s
                                                 if rest == "'" then [(c, t)] else []
                                         else []
                                []    -> [])

    readList = readParen False
                 (\r -> do (tok, t) <- lex r
                           case tok of
                             (q:s) -> if q == '"'
                                      then do (l, _) <- readString s
                                              [(l, t)]
                                      else []
                             []    -> [])
        where readString s = case s of
                               (c:d:t) -> if c == '\\' && d == '&'
                                          then readString t
                                          else readStringChar s
                               _       -> readStringChar s
              readStringChar s = case s of
                                   (c:t) -> if c == '"'
                                            then [([], t)]
                                            else do (ch, u) <- readLitChar s
                                                    (cs, v) <- readString u
                                                    return (ch : cs, v)
                                   []    -> []

instance (Read a) => Read [a] where
    readsPrec _ = readList

instance Read Bool where
    readsPrec _ = readParen False
                    (\r -> do (tok, t) <- lex r
                              if tok == "True" then [(True, t)]
                              else if tok == "False" then [(False, t)]
                              else [])

instance Read Ordering where
    readsPrec _ = readParen False
                    (\r -> do (tok, t) <- lex r
                              if tok == "LT" then [(LT, t)]
                              else if tok == "EQ" then [(EQ, t)]
                              else if tok == "GT" then [(GT, t)]
                              else [])

instance Read () where
    readsPrec _ = readParen False
                    (\r -> do s <- readToken "(" r
                              t <- readToken ")" s
                              return ((), t))

instance (Read a) => Read (Maybe a) where
    readsPrec d r =
        readParen False (\r -> do t <- readToken "Nothing" r
                                  return (Nothing, t)) r
        ++ readParen (d > 10) (\r -> do t <- readToken "Just" r
                                        (x, u) <- readsPrec 11 t
                                        return (Just x, u)) r

instance (Read a, Read b) => Read (Either a b) where
    readsPrec d r =
        readParen (d > 10) (\r -> do t <- readToken "Left" r
                                     (x, u) <- readsPrec 11 t
                                     return (Left x, u)) r
        ++ readParen (d > 10) (\r -> do t <- readToken "Right" r
                                        (y, u) <- readsPrec 11 t
                                        return (Right y, u)) r

-- A tuple's components, after its opening parenthesis: each followed by
-- a comma, the last by the closing parenthesis.
readComponent :: Read a => String -> String -> [(a, String)]
readComponent close s = do (x, t) <- reads s
                           u <- readToken close t
                           return (x, u)

instance (Read a1, Read a2) => Read (a1,a2) where
    readsPrec _ = readParen False
                    (\r -> do s0 <- readToken "(" r
                              (x1, s1) <- readComponent "," s0
                              (x2, s2) <- readComponent ")" s1
                              return ((x1,x2), s2))

instance (Read a1, Read a2, Read a3) => Read (a1,a2,a3) where
    readsPrec _ = readParen False
                    (\r -> do s0 <- readToken "(" r
                              (x1, s1) <- readComponent "," s0
                              (x2, s2) <- readComponent "," s1
                              (x3, s3) <- readComponent ")" s2
                              return ((x1,x2,x3), s3))

instance (Read a1, Read a2, Read a3, Read a4) => Read (a1,a2,a3,a4) where
    readsPrec _ = readParen False
                    (\r -> do s0 <- readToken "(" r
                              (x1, s1) <- readComponent "," s0
                              (x2, s2) <- readComponent "," s1
                              (x3, s3) <- readComponent "," s2
                              (x4, s4) <- readComponent ")" s3
                              return ((x1,x2,x3,x4), s4))

instance (Read a1, Read a2, Read a3, Read a4, Read a5) => Read (a1,a2,a3,a4,a5) where
    readsPrec _ = readParen False
                    (\r -> do s0 <- readToken "(" r
                              (x1, s1) <- readComponent "," s0
                              (x2, s2) <- readComponent "," s1
                              (x3, s3) <- readComponent "," s2
                              (x4, s4) <- readComponent "," s3
                              (x5, s5) <- readComponent ")" s4
                              return ((x1,x2,x3,x4,x5), s5))

instance (Read a1, Read a2, Read a3, Read a4, Read a5, Read a6) => Read (a1,a2,a3,a4,a5,a6) where
    readsPrec _ = readParen False
                    (\r -> do s0 <- readToken "(" r
                              (x1, s1) <- readComponent "," s0
                              (x2, s2) <- readComponent "," s1
                              (x3, s3) <- readComponent "," s2
                              (x4, s4) <- readComponent "," s3
                              (x5, s5) <- readComponent "," s4
                              (x6, s6) <- readComponent ")" s5
                              return ((x1,x2,x3,x4,x5,x6), s6))

instance (Read a1, Read a2, Read a3, Read a4, Read a5, Read a6, Read a7) => Read (a1,a2,a3,a4,a5,a6,a7) where
    readsPrec _ = readParen False
                    (\r -> do s0 <- readToken "(" r
                              (x1, s1) <- readComponent "," s0
                              (x2, s2) <- readComponent "," s1
                              (x3, s3) <- readComponent "," s2
                              (x4, s4) <- readComponent "," s3
                              (x5, s5) <- readComponent "," s4
                              (x6, s6) <- readComponent "," s5
                              (x7, s7) <- readComponent ")" s6
                              return ((x1,x2,x3,x4,x5,x6,x7), s7))
