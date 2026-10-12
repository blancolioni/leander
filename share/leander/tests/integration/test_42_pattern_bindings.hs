module test_42_pattern_bindings where

--  Every name here is prefixed: the integration suite shares one handle,
--  so a module's top-level names stay visible to later modules, and a
--  clash silently resolves to the earlier definition rather than failing.

--  A binding's left-hand side may be a pattern (#112).

pbSwapSum :: (Int, Int) -> Int
pbSwapSum p = a + b
  where (a, b) = p

pbLet :: Int
pbLet = let (x, y) = (3, 4) in x * y

data PbBox = PbBox Int Int

pbBoxed :: Int
pbBoxed = s
  where PbBox s _ = PbBox 5 6

pbConsed :: Int
pbConsed = h
  where h : _ = [7, 8, 9]

--  The Prelude's own style: the recursive call's result taken apart.
pbUnzip3 :: [(a, b, c)] -> ([a], [b], [c])
pbUnzip3 [] = ([], [], [])
pbUnzip3 ((a, b, c) : rest) = (a : as, b : bs, c : cs)
  where (as, bs, cs) = pbUnzip3 rest

--  Lazy: a component nothing uses is never evaluated.
pbLazy :: Int
pbLazy = let (used, unused) = (1, error "never forced") in used

(pbTopA, pbTopB) = (10, 20)

pbTuple     = pbSwapSum (1, 2) == 3 && pbLet == 12
pbCons      = pbBoxed == 5 && pbConsed == 7
pbRecursive = pbUnzip3 [(1, 'a', True), (2, 'b', False)]
                == ([1, 2], "ab", [True, False])
pbLazily    = pbLazy == 1
pbTopLevel  = pbTopA + pbTopB == 30
