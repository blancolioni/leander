module test_21_recursive_bindings where

--  Every name here is prefixed: the integration suite shares one handle,
--  so a module's top-level names stay visible to later modules, and a
--  clash silently resolves to the earlier definition rather than failing.

--  A binding with no arguments that refers to itself has to be tied with
--  Y like any other recursive binding.

rbOnes = rbSum (take 3 rbXs)
  where rbXs = 1 : rbXs

rbSum :: [Int] -> Int
rbSum [] = 0
rbSum (x:xs) = x + rbSum xs

--  A class constraint used only inside a case alternative still has to
--  reach the binding's dictionary parameter.

rbInAlt :: Eq a => a -> Bool -> Int
rbInAlt x b = case b of
  True  -> if x == x then 1 else 0
  False -> 2

rbAlt = rbInAlt 'c' True

--  A recursive binding with a declared constraint applies its dictionary
--  at its own recursive call, so its name has to denote the binding
--  outside the dictionary lambda.

rbFind :: Eq a => a -> [a] -> Int
rbFind k [] = 0
rbFind k (x:xs) = if x == k then 1 else rbFind k xs

rbFound   = rbFind 3 [1,2,3]
rbMissing = rbFind 4 [1,2,3]

--  The same, local to a let; and a non-recursive local binding with a
--  declared constraint, which must still take its dictionary.

rbLocalFind :: Int -> Int
rbLocalFind w =
  let { go :: Eq a => a -> [a] -> Int
      ; go k [] = 0
      ; go k (x:xs) = if x == k then 1 else go k xs
      } in go w [1,2,3]

rbLocalFound   = rbLocalFind 2
rbLocalMissing = rbLocalFind 5

rbLocalEq :: Int -> Int
rbLocalEq w =
  let { same :: Eq a => a -> a -> Int
      ; same x y = if x == y then 1 else 0
      } in same w 7

rbLocalSame = rbLocalEq 7
