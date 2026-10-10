module test_31_monad where

--  Every name here is prefixed: the integration suite shares one handle,
--  so a module's top-level names stay visible to later modules, and a
--  clash silently resolves to the earlier definition rather than failing.

--  Applicative is Monad's superclass, and return defaults to pure
--  (issue #125).

--  A user's monad needs only (>>=) beyond its Applicative instance.

data MnBox a = MnBox a

mnUnbox (MnBox x) = x

instance Functor MnBox where
    fmap f (MnBox x) = MnBox (f x)

instance Applicative MnBox where
    pure = MnBox
    (MnBox f) <*> (MnBox x) = MnBox (f x)

instance Monad MnBox where
    (MnBox x) >>= f = f x

mnBoxReturn = mnUnbox (return 1 >>= \x -> MnBox (x + 1)) == 2
mnBoxDo     = mnUnbox (do { x <- MnBox 20; y <- pure 22; return (x + y) }) == 42

--  Lists are a monad, not only an applicative.

mnListBind  = sum ([1, 2] >>= \x -> [x, x * 10]) == 33
mnListDo    = sum (do { x <- [1, 2, 3]; y <- [10, 20]; return (x * y) }) == 180

--  The Prelude's monads lost their own return to the default.

mnMaybe     = (return 3 >>= \x -> Just (x + 1)) == Just 4
mnIO        = runIO (return 5 >>= \x -> return (x + 1)) == 6
mnIOAp      = runIO (pure (+ 1) <*> pure 41) == 42
