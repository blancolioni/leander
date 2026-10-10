module test_27_tuples where

--  Every name here is prefixed: the integration suite shares one handle,
--  so a module's top-level names stay visible to later modules, and a
--  clash silently resolves to the earlier definition rather than failing.

--  Tuples larger than pairs, in types, expressions and patterns
--  (issue #80).

tpSum3 :: (Int, Int, Int) -> Int
tpSum3 (a, b, c) = a + b * c

tpSum3Value = tpSum3 (1, 2, 3)

tpSeven :: (Int, Int, Int, Int, Int, Int, Int) -> Int
tpSeven (a, b, c, d, e, f, g) = a + b + c + d + e + f + g

tpSevenValue = tpSeven (1, 2, 3, 4, 5, 6, 7)

--  The largest size the Prelude scaffold registers.

tpLast (_, _, _, _, _, _, _, _, _, _, _, _, _, _, o) = o

tpFifteen = tpLast (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)

--  A tuple constructor on its own.

tpBuilt = tpSum3 ((,,) 1 2 3)

--  The Prelude functions it unlocks.

tpZip3 = length (zip3 [1, 2, 3] "ab" [True, False, True])

tpZipWith3 = sum (zipWith3 (\a -> \b -> \c -> a * b + c) [1, 2, 3] [4, 5, 6] [7, 8, 9])

tpThird (_, _, z) = z

tpUnzip3 = length (tpThird (unzip3 [(1, 'a', True), (2, 'b', False)]))

--  unzip builds its lists lazily, so it copes with an infinite one.

tpUnzipLazy = sum (take 3 (fst (unzip (zip [1 ..] [10 ..]))))

--  Eq, Ord and Show instances.

tpEqual    = (1, 'a', True) == (1, 'a', True)
tpUnequal  = (1, 'a', True) == (1, 'b', True)
tpLess     = (1, 2, 3) < (1, 3, 0)
tpCompare  = compare (2, 1) (1, 9) == GT
tpMax      = max (1, 9) (2, 0) == (2, 0)
tpShown    = show (1, True, (2, False)) == "(1,True,(2,False))"
tpSevenEq  = (1, 2, 3, 4, 5, 6, 7) == (1, 2, 3, 4, 5, 6, 7)

--  An instance whose context has two predicates of the same class.  Its
--  dictionary used to merge their type variables and take its
--  parameters in the wrong order, so every use gave garbage.

class TpSame a where
    tpSame :: a -> a -> Bool

data TpTwo a b = TpTwo a b

instance (Eq a, Eq b) => TpSame (TpTwo a b) where
    tpSame (TpTwo a1 a2) (TpTwo b1 b2) = a1 == b1 && a2 == b2

tpTwoSame      = tpSame (TpTwo 1 'a') (TpTwo 1 'a')
tpTwoDifferent = tpSame (TpTwo 1 'a') (TpTwo 1 'b')
