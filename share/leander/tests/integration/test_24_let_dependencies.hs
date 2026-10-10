module test_24_let_dependencies where

--  Every name here is prefixed: the integration suite shares one handle,
--  so a module's top-level names stay visible to later modules, and a
--  clash silently resolves to the earlier definition rather than failing.

--  A local binding sees its siblings whatever order they are written in.

ldChain = c
  where c = b + 1
        b = a * 2
        a = 5

--  Mutually recursive local bindings.

ldCycle = sum (take 5 xs)
  where xs = 1 : ys
        ys = 2 : xs

ldParity n = ev n
  where ev 0 = True
        ev k = od (k - 1)
        od 0 = False
        od k = ev (k - 1)

ldEven = ldParity 10
ldOdd  = ldParity 7

--  The same, under a class constraint that only one of them raises: the
--  pair shares its dictionary, since neither applies one to the other.

ldAlternates x ys = e ys
  where e [] = True
        e (y:rest) = x == y && o rest
        o [] = False
        o (y:rest) = e rest

ldEvenMatch = ldAlternates 'a' "ab"
ldOddMatch  = ldAlternates 'a' "aba"

ldLocalAlt = let { e x [] = True
                 ; e x (y:ys) = x == y && o x ys
                 ; o x [] = False
                 ; o x (y:ys) = e x ys
                 } in e 1 [1,2] && not (e True [True])

--  A top-level binding is generalised before the ones that use it are
--  inferred, so they can use it at more than one type.

ldId x = x

ldPair = (ldId 1, ldId True)

ldPolyUse = fst ldPair == 1 && snd ldPair
