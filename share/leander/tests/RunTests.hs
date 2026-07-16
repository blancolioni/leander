module RunTests where

showResult :: Bool -> [Char]
showResult True = "PASS"
showResult False = "FAIL"

runTest :: [Char] -> Bool -> IO Int
runTest name result = do
    putStrLn $ showResult result ++ "    " ++ name
    return (if result then 1 else 0)

tests :: [([Char], Bool)]
tests = 
    [  ("(+1) 3 == 4", (+1) 3 == 4)
    , ("map (+1) [1,2,3] == [2,3,4]", map (+1) [1,2,3] == [2,3,4])
    , ("take 2 [1,2,3,4] == [1,2]", take 2 [1,2,3,4] == [1,2])
    , ("sum [1,2,3] == 6", sum [1,2,3] == 6)
    , ("foldl (+) 0 [1,2,3] == 6", foldl (+) 0 [1,2,3] == 6)
    , ("foldr (+) 0 [1,2,3] == 6", foldr (+) 0 [1,2,3] == 6)
    , ("concat [[1,2],[3,4]] == [1,2,3,4]", concat [[1,2],[3,4]] == [1,2,3,4])
    , ("[1,2] ++ [3,4] == [1,2,3,4]", [1,2] ++ [3,4] == [1,2,3,4])
    , ("fst (3,4) == 3", fst (3,4) == 3)
    , ("snd (3,4) == 4", snd (3,4) == 4)
    , ("foldr (+) 0 [1,2,3] == 6", foldr (+) 0 [1,2,3] == 6)
    , ("let x = 3 in x + 1 == 4", (let x = 3 in x + 1) == 4)
    , ("case 3 of { 1 -> 10; 2 -> 20; _ -> 30 } == 30", (case 3 of { 1 -> 10; 2 -> 20; _ -> 30 }) == 30)
    , ("if True then 5 else 10 == 5", (if True then 5 else 10) == 5)
    , ("if False then 5 else 10 == 10", (if False then 5 else 10) == 10)
    , ("True == True", True == True)
    , ("False == False", False == False)
    , ("True /= False", True /= False)
    , ("False /= True", False /= True)
    , ("not True == False", not True == False)
    , ("not False == True", not False == True)
    , ("True && True == True", True && True == True)
    , ("True && False == False", True && False == False)
    , ("False && True == False", (False && True) == False)
    , ("False && False == False", (False && False) == False)
    , ("True || True == True", True || True == True)
    , ("True || False == True", True || False == True)
    , ("False || True == True", False || True == True)
    , ("False || False == False", False || False == False)
    , ("1 == 1", 1 == 1)
    , ("1 /= 2", 1 /= 2)
    , ("1 < 2", 1 < 2)
    , ("2 > 1", 2 > 1)
    , ("1 <= 1", 1 <= 1)
    , ("1 >= 1", 1 >= 1)
    , ("0 < 0 == False", not (0 < 0))
    , ("0 > 0 == False", not (0 > 0))
    , ("0 <= 0 == True", 0 <= 0)
    , ("0 >= 0 == True", 0 >= 0)
    , ("[1] == [1]", [1] == [1])
    , ("[1] /= [2]", [1] /= [2])
    , ("max 1 2 == 2", max 1 2 == 2)
    , ("min 1 2 == 1", min 1 2 == 1)
    , ("filter (<3) [1,2,3,4] == [1,2]", filter (<3) [1,2,3,4] == [1,2])
    , ("sum [1 .. 10] == 55", sum [1 .. 10] == 55)
    , ("[2, 4 .. 10]", [2, 4 .. 10] == [2, 4, 6, 8, 10])
    , ("[1 ..]", sum (take 10 [1 .. ]) == 55)
    , ("[1, 3 .. 10]", [1, 3 .. 10] == [1, 3, 5, 7, 9])
    , ("sum (drop 5 [1 .. 10]) == 40", sum (drop 5 [1 .. 10]) == 40)
    , ("reverse [] == []", null (reverse []))
    , ("reverse [1,2,3] == [3,2,1]", reverse [1,2,3] == [3,2,1])
    , ("fmap (+1) (Just 3) == Just 4", fmap (+1) (Just 3) == Just 4)
    , ("fmap (+1) Nothing == Nothing", fmap (+1) Nothing == Nothing)
    , ("Just 3 == Just 3", Just 3 == Just 3)
    , ("Just 3 /= Just 4", Just 3 /= Just 4)
    , ("Nothing /= Just 3", Nothing /= Just 3)
    , ("fmap (+1) [1,2,3] == [2,3,4]", fmap (+1) [1,2,3] == [2,3,4])
    , ("pure 3 == [3]", pure 3 == [3])
    , ("([(+1), (*2)] <*> ([1,2]) == [2,3,2,4]", ([(+1), (*2)] <*> [1,2]) == [2,3,2,4])
    , ("x where x = 3", test_where_1 == 3)
    , ("x + 1 where x = 3", test_where_2 == 4)
    , ("zip", sum (map (\x -> fst x + snd x) (zip [1,2,3] [4,5,6])) == 21)
    , ("zipWith", sum (zipWith (+) [1,2,3] [4,5,6]) == 21)
    , ("takeWhile", takeWhile (< 5) [1 .. ] == [1,2,3,4])
    , ("dropWhile", dropWhile (< 5) [1 .. 10] == [5,6,7,8,9,10])
    , ("elem", elem 3 [1,2,3,4] == True)
    , ("notElem", notElem 5 [1,2,3,4] == True)
    , ("all", all (< 5) [1,2,3,4] == True)
    , ("any", any (> 3) [1,2,3,4] == True)
    , ("span", fst (span (< 5) [1 ..]) == [1,2,3,4])
    , ("break", fst (break (> 5) [1 ..]) == [1,2,3,4,5])
    ]

test_where_1 :: Int
test_where_1 = x where x = 3
test_where_2 :: Int
test_where_2 = x + 1 where x = 3

main :: IO ()
-- main = mapM_ (uncurry runTest) tests

main = do results <- mapM (uncurry runTest) tests
          putStrLn $ "Tests: " ++ show (length results)
                          ++ "; passed " ++ show (sum results)
                          ++ "; failed " ++ show (length results - sum results)
