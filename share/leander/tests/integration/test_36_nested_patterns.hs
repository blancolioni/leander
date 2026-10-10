module test_36_nested_patterns where

--  Every name here is prefixed: the integration suite shares one handle,
--  so a module's top-level names stay visible to later modules, and a
--  clash silently resolves to the earlier definition rather than failing.

--  Patterns nest to any depth (issue #109).

npJust :: Maybe (Maybe Int) -> Int
npJust (Just (Just a)) = a
npJust _               = 0

npJusts = npJust (Just (Just 1)) == 1 && npJust (Just Nothing) == 0
            && npJust Nothing == 0

npSecond :: [Int] -> Int
npSecond (x:y:rest) = y
npSecond _          = 0

npCons = npSecond [1, 2, 3] == 2 && npSecond [1] == 0

npSum :: Maybe (Int, Int) -> Int
npSum (Just (a, b)) = a + b
npSum Nothing       = 0

npTuple = npSum (Just (1, 2)) == 3

--  A literal inside a constructor, falling through to the next equation.
npZero :: Maybe Int -> Int
npZero (Just 0) = 10
npZero (Just n) = n
npZero Nothing  = 0

npLiteral = npZero (Just 0) == 10 && npZero (Just 5) == 5

--  List patterns, including a single element.
npList :: [Int] -> Int
npList []     = 0
npList [x]    = x
npList [a, b] = a * b
npList _      = 1

npLists = npList [] == 0 && npList [7] == 7 && npList [3, 4] == 12
            && npList [1, 2, 3] == 1

npPairs :: [(Int, Int)] -> Int
npPairs ((a, b):rest) = a + b
npPairs []            = 0

npInList = npPairs [(1, 2), (3, 4)] == 3

--  In case alternatives and lambdas, which become equations too.
npCase = (case Just (Just 3) of { Just (Just x) -> x; _ -> 0 }) == 3
npLambda = (\(a, (b, c)) -> a + b + c) (1, (2, 3)) == 6

--  Nested beside another column, falling through in either.
npTwo :: [Int] -> Int -> Int
npTwo (x:(y:_)) 0 = x
npTwo (_:(y:_)) n = y
npTwo _         _ = 0

npColumns = npTwo [5, 6] 0 == 5 && npTwo [5, 6] 1 == 6 && npTwo [5] 3 == 0
