module RunTests where

showResult :: Bool -> [Char]
showResult True = "PASS"
showResult False = "FAIL"

runTest :: [Char] -> Bool -> IO ()
runTest name result = do
    putStrLn $ showResult result ++ "    " ++ name
    return ()

tests :: [([Char], Bool)]
tests = [
    ("succ 3 == 4", succ 3 == 4),
    ("map (+1) [1,2,3] == [2,3,4]", sum (map succ [1,2,3]) == 9),
    ("take 2 [1,2,3,4] == [1,2]", sum (take 2 [1,2,3,4]) == 3),
    ("sum [1,2,3] == 6", sum [1,2,3] == 6),
    ("concat [[1,2],[3,4]] == [1,2,3,4]", sum (concat [[1,2],[3,4]]) == 10),
    ("[1,2] ++ [3,4] == [1,2,3,4]", sum ([1,2] ++ [3,4]) == 10),
    ("zero 0 == True", zero 0),
    ("zero 5 == False", not (zero 5)),
    ("small 2 == True", small 2),
    ("small 5 == False", not (small 5)),
    ("fst (3,4) == 3", fst (3,4) == 3),
    ("snd (3,4) == 4", snd (3,4) == 4),
    ("foldr (+) 0 [1,2,3] == 6", foldr (+) 0 [1,2,3] == 6)
]

main :: IO ()
main = mapM_ (uncurry runTest) tests
