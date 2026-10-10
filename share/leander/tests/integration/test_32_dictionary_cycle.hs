module test_32_dictionary_cycle where

--  Every name here is prefixed: the integration suite shares one handle,
--  so a module's top-level names stay visible to later modules, and a
--  clash silently resolves to the earlier definition rather than failing.

--  Instance dictionaries that refer to each other.  Linking one used to
--  link the other first, and so on forever (issue #130).

data DcBox a = DcBox a

dcUnbox (DcBox x) = x

instance Functor DcBox where
    fmap f (DcBox x) = DcBox (f x)

--  (<*>) uses Monad DcBox, whose return is this instance's pure.
instance Applicative DcBox where
    pure = DcBox
    mf <*> mx = do
        f <- mf
        x <- mx
        return (f x)

instance Monad DcBox where
    (DcBox x) >>= f = f x

dcAp     = dcUnbox (pure (+ 1) <*> DcBox 41) == 42
dcReturn = dcUnbox (return 1 >>= \x -> DcBox (x + 1)) == 2

--  Three in a ring, through ordinary functions as well as dictionaries.

class DcA a where
    dcA :: a -> Int

class DcB a where
    dcB :: a -> Int

instance DcA Bool where
    dcA b = if b then 1 else dcViaB b

instance DcB Bool where
    dcB b = if b then dcA b + 10 else 0

dcViaB :: Bool -> Int
dcViaB b = dcB b + 100

dcRing = dcA True == 1 && dcA False == 100 && dcB True == 11
