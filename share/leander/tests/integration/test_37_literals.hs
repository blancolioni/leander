module test_37_literals where

--  Every name here is prefixed: the integration suite shares one handle,
--  so a module's top-level names stay visible to later modules, and a
--  clash silently resolves to the earlier definition rather than failing.

--  An integer literal is fromInteger n, at whatever Num type the context
--  gives it, defaulting to Int when nothing does (issue #82).

--  A Num type of our own: literals at it go through its fromInteger.
data OlMod7 = OlMod7 Int

olValue (OlMod7 n) = n

olReduce :: Int -> OlMod7
olReduce n = OlMod7 (mod n 7)

instance Eq OlMod7 where
    (OlMod7 a) == (OlMod7 b) = a == b

instance Show OlMod7 where
    show (OlMod7 n) = show n ++ " (mod 7)"

instance Num OlMod7 where
    (OlMod7 a) + (OlMod7 b) = olReduce (a + b)
    (OlMod7 a) * (OlMod7 b) = olReduce (a * b)
    (OlMod7 a) - (OlMod7 b) = olReduce (a - b + 7)
    negate (OlMod7 a)       = olReduce (7 - a)
    abs x                   = x
    signum x                = 1
    fromInteger n           = olReduce (fromInteger n)

olTen :: OlMod7
olTen = 10

olLiteral  = olValue olTen == 3
olArith    = olValue (olReduce 5 + 4) == 2
olShown    = show (olReduce 5 * 3) == "1 (mod 7)"

--  A literal pattern at that type.
olIsZero :: OlMod7 -> Bool
olIsZero 0 = True
olIsZero _ = False

olPattern  = olIsZero (olReduce 14) && not (olIsZero (olReduce 15))

--  A function over any Num, with a literal and a literal pattern.
olFact 0 = 1
olFact n = n * olFact (n - 1)

olGeneric  = olFact 5 == 120 && olValue (olFact (olReduce 4)) == 3

--  Ambiguous: nothing fixes the type of 3, so it defaults to Int.
olDefault  = show 3 == "3" && show (2 + 3) == "5"
olRange    = [1 .. 5] == [1, 2, 3, 4, 5]
