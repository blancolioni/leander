module test_48_prelude_functions where

--  Every name here is prefixed: the integration suite shares one handle,
--  so a module's top-level names stay visible to later modules, and a
--  clash silently resolves to the earlier definition rather than failing.

--  The Prelude functions and instances #79 still listed.

pfRight1, pfRight2 :: Either Char Int
pfRight1 = Right 1
pfRight2 = Right 2

--  Ord for lists, Maybe and Either.
pfOrd = compare [1, 2] [1, 3] == LT && compare [1, 2] [1] == GT
        && compare [] [0] == LT
        && compare (Just 1) Nothing == GT && compare Nothing (Just 0) == LT
        && compare (Left 'a') pfRight1 == LT && compare pfRight2 pfRight1 == GT
        && maximum [Just 3, Nothing, Just 5] == Just 5

pfLists = concatMap (\x -> [x, x]) [1, 2] == [1, 1, 2, 2]
          && scanl (+) 0 [1, 2, 3] == [0, 1, 3, 6]
          && scanl1 max [3, 1, 4] == [3, 3, 4]
          && scanr (+) 0 [1, 2, 3] == [6, 5, 3, 0]
          && scanr1 (+) [1, 2, 3] == [6, 5, 3]
          && foldr1 (-) [10, 3, 2] == 9
          && splitAt 2 [1, 2, 3, 4, 5] == ([1, 2], [3, 4, 5])

pfText = lines "one\ntwo\n\nfour" == ["one", "two", "", "four"]
         && words "  hello   big\tworld\n" == ["hello", "big", "world"]
         && unlines ["a", "b"] == "a\nb\n"
         && unwords ["a", "b", "c"] == "a b c"
         && span (< 3) [1, 2, 3, 4] == ([1, 2], [3, 4])

--  div and mod round down, quot and rem towards zero.
pfDivision = div (negate 7) 2 == negate 4 && mod (negate 7) 2 == 1
             && quot (negate 7) 2 == negate 3 && rem (negate 7) 2 == negate 1
             && divMod 7 (negate 2) == (negate 4, negate 1)
             && quotRem 7 (negate 2) == (negate 3, 1)

pfNumbers = even 4 && odd 3 && not (odd 4)
            && gcd 12 18 == 6 && gcd (negate 4) 6 == 2
            && lcm 4 6 == 12 && lcm 0 3 == 0
            && 2 ^ 10 == 1024 && 3 ^ 0 == 1 && negate 2 ^ 3 == negate 8
            && fromIntegral (length "abc") + 1 == 4

--  sum and product at a type of the module's own.
data PfMod5 = PfMod5 Int deriving (Eq, Show)

instance Num PfMod5 where
    PfMod5 a + PfMod5 b = PfMod5 (mod (a + b) 5)
    PfMod5 a * PfMod5 b = PfMod5 (mod (a * b) 5)
    PfMod5 a - PfMod5 b = PfMod5 (mod (a - b) 5)
    negate (PfMod5 a)   = PfMod5 (mod (negate a) 5)
    abs x               = x
    signum _            = 1
    fromInteger n       = PfMod5 (mod (fromInteger n) 5)

pfGeneric = sum [1, 2, 3] == 6 && product [1, 2, 3, 4] == 24
            && sum [PfMod5 3, PfMod5 4] == PfMod5 2
            && product [PfMod5 2, PfMod5 3] == PfMod5 1
            && subtract (PfMod5 1) (PfMod5 0) == PfMod5 4

pfOther = until (> 100) (* 2) 1 == 128
          && asTypeOf 3 (length "four") == 3
          && ((\x -> [x, x + 1]) =<< [10, 20]) == [10, 11, 20, 21]
          && ((+ 1) <$> Just 1) == Just 2
          && (0 <$ Just 'x') == Just 0
          && (Just 1 *> Just 2) == Just 2 && (Just 1 <* Just 2) == Just 1
          && ([1, 2] <* "ab") == [1, 1, 2, 2]

--  seq, $! and foldl' on values with fields: the machine used to hand
--  back something built from seq's first argument (skit, strict partial
--  applications).
pfStrict = seq (Just 1) (Just 2) == Just 2
           && (id $! PfMod5 3) == PfMod5 3
           && foldl' (+) (PfMod5 0) [PfMod5 3, PfMod5 4] == PfMod5 2
