module test_12_full where

showResult :: Bool -> [Char]
showResult True = "PASS"
showResult False = "FAIL"

runTest :: [Char] -> Bool -> IO ()
runTest name result =
    putStrLn $ showResult result ++ " " ++ name

tests :: [([Char], Bool)]
tests = [("1+1==2", 1 + 1 == 2),
         ("True==True", True == True)]

main :: IO ()
main = mapM_ (uncurry runTest) tests
