module test_18_newtype where

newtype Age = Age Int

newtype Wrap a = Wrap a

newtype Pair = Pair (Int, Int)

unAge :: Age -> Int
unAge (Age n) = n

unwrap :: Wrap a -> a
unwrap (Wrap x) = x

bump :: Age -> Age
bump a = case a of
           Age n -> Age (n + 1)

older :: Age -> Age -> Bool
older (Age a) (Age b) = a > b

fstPair :: Pair -> Int
fstPair (Pair p) = fst p

-- A newtype value is its field, so wrapping and unwrapping are both
-- the identity at runtime: these must all evaluate to bare values.
bare      = Age 65
unwrapped = unAge (Age 65)
bumped    = unAge (bump (Age 65))
nested    = unAge (unwrap (Wrap (Age 7)))
compared  = older (Age 70) (Age 65)
inTuple   = fstPair (Pair (3, 4))

newtype Name = Name Int deriving Eq

newtype Fn = Fn (Int -> Int)

applyFn :: Fn -> Int -> Int
applyFn (Fn f) x = f x

class Describe a where
  describe :: a -> Int

instance Describe Age where
  describe (Age n) = n

derivedEq  = Name 3 == Name 3
derivedNe  = Name 3 == Name 4
viaClass   = describe (Age 9)
funcField  = applyFn (Fn (\x -> x + x)) 21
