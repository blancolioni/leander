module test_26_signatures_clean where

--  Signatures the checks must accept.

--  Eq a follows from Ord a by superclass.

scSuper :: Ord a => a -> a -> Bool
scSuper x y = x == y

--  Eq [a] follows from Eq a by instance.

scInstance :: Eq a => [a] -> [a] -> Bool
scInstance xs ys = xs == ys

--  A declared context may say more than the body needs.

scUnused :: Eq a => a -> a
scUnused x = x

--  A concrete predicate is not the signature's business.

scConcrete :: Int -> Bool
scConcrete n = n == n

--  A polymorphic primitive, used through a polymorphic signature.

scSeq :: a -> b -> b
scSeq = seq

--  A local signature, at a type the enclosing one fixes.

scLocal :: Int -> Bool
scLocal n = let { g :: Ord a => a -> a -> Bool; g x y = x < y } in g n n
