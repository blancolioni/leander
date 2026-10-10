module test_25_toplevel_mutual_recursion where

--  Every name here is prefixed: the integration suite shares one handle,
--  so a module's top-level names stay visible to later modules, and a
--  clash silently resolves to the earlier definition rather than failing.

--  Top-level bindings that refer to each other.  Each used to be compiled
--  and installed on its own, and installing one needed the other already
--  installed, so the pair recursed until the stack overflowed (issue #102).

tmEven 0 = True
tmEven n = tmOdd (n - 1)

tmOdd 0 = False
tmOdd n = tmEven (n - 1)

--  The same through a declared type: a cycle of references, though not
--  one inference sees, since an explicit binding's type is already known.

tmEvenSig :: Int -> Bool
tmEvenSig 0 = True
tmEvenSig n = tmOddSig (n - 1)

tmOddSig 0 = False
tmOddSig n = tmEvenSig (n - 1)

--  Under a class constraint that only one of the pair raises: they share
--  its dictionary, since neither applies one to the other.

tmAlt x [] = True
tmAlt x (y:ys) = x == y && tmAltRest x ys

tmAltRest x [] = False
tmAltRest x (y:ys) = tmAlt x ys

--  And through a constrained declared type, whose recursive uses do apply
--  dictionaries.

tmAltSig :: Eq a => a -> [a] -> Bool
tmAltSig x [] = True
tmAltSig x (y:ys) = x == y && tmAltSigRest x ys

tmAltSigRest x [] = False
tmAltSigRest x (y:ys) = tmAltSig x ys
