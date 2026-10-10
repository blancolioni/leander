module test_33_missing_superclass where

--  A Monad instance needs an Applicative instance for the same type: it
--  is the superclass, and the dictionary carries it (issue #129).

data MsBox a = MsBox a

instance Monad MsBox where
    (MsBox x) >>= f = f x
