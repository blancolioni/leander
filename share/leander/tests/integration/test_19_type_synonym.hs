module test_19_type_synonym where

type Count    = Int
type Duo a    = (a, a)
type Assoc k v = [(k, v)]
type Both     = Duo Int

-- A synonym is transparent: Count and Int are the same type, so a
-- function declared with one accepts a value produced by the other.
bump :: Count -> Count
bump n = n + 1

plainInt :: Int -> Int
plainInt n = n

viaSynonym = plainInt (bump 41)

dup :: a -> Duo a
dup x = (x, x)

dupped = fst (dup 7) + snd (dup 7)

firstKey :: Assoc Int Char -> Int
firstKey xs = fst (head xs)

keyed = firstKey [(3, 'a'), (4, 'b')]

-- one synonym written in terms of another
swap2 :: Both -> Both
swap2 p = (snd p, fst p)

swapped = fst (swap2 (1, 2))

-- String comes from the Prelude, so naming it here also proves a synonym
-- crosses a module boundary
shout :: String -> Count
shout s = length s

shouted = shout ['a', 'b', 'c']

-- in a data field, a newtype field, a class signature and an instance head
data Person = Person String Count

ageOf :: Person -> Count
ageOf (Person _ a) = a

aged = ageOf (Person ['x'] 44)

newtype Tag = Tag String

class Sized a where
  size :: a -> Count

instance Sized Tag where
  size (Tag s) = length s

sized = size (Tag ['a', 'b'])
