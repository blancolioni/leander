module test_33_superclasses where

--  Every name here is prefixed: the integration suite shares one handle,
--  so a module's top-level names stay visible to later modules, and a
--  clash silently resolves to the earlier definition rather than failing.

--  A dictionary carries its superclasses' dictionaries, so a constraint
--  provides its superclasses too (issue #129).

--  Ord a provides Eq a.
scSame :: Ord a => a -> a -> Bool
scSame x y = x == y

scSameInt  = scSame 3 3
scSameChar = not (scSame 'a' 'b')

--  ... and Eq [a], through Eq a and the list instance.
scLists :: Ord a => [a] -> [a] -> Bool
scLists xs ys = xs == ys

scListsEqual = scLists [1, 2] [1, 2]

--  Num a provides Show a (Num's superclasses are Eq and Show).
scShowNum :: Num a => a -> String
scShowNum x = show (x + x)

scShown = scShowNum 21 == "42"

--  Monad m provides Applicative m (#125).
scTwice :: Monad m => m a -> m (a, a)
scTwice m = pure (,) <*> m <*> m

scMonad = scTwice (Just 4) == Just (4, 4)

--  A declared context the body does not use, and two predicates used in
--  the opposite order to their declaration: an explicit binding takes its
--  dictionaries in declared order.
scUnused :: Eq a => a -> a
scUnused x = x

scBoth :: (Show a, Eq a) => a -> a -> String
scBoth x y = if x == y then show x else "no"

scDeclared = scUnused 5 == 5 && scBoth 7 7 == "7"

--  An instance whose context is Ord a, needing Eq a.
data ScBox a = ScBox a

instance (Eq a) => Eq (ScBox a) where
    (ScBox x) == (ScBox y) = x == y

instance (Ord a) => Ord (ScBox a) where
    compare (ScBox x) (ScBox y) = if x == y then EQ else compare x y

scInstance = compare (ScBox 1) (ScBox 1) == EQ && ScBox 1 < ScBox 2

--  Ord a, through the instance for ScBox a, to Eq a.
scBoxSame :: Ord a => a -> a -> Bool
scBoxSame x y = ScBox x == ScBox y

scThroughInstance = scBoxSame 4 4
