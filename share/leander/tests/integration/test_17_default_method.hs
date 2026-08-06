module test_17_default_method where

data Foo = Foo Int

instance Eq Foo where
  (Foo a) == (Foo b) = a == b
