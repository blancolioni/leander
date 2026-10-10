module test_26_too_general where

--  The body fixes the signature's type variable: a is Bool here.

sgTooGeneral :: a -> a
sgTooGeneral x = True
