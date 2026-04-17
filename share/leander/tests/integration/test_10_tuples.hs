module test_10_tuples where
main :: IO ()
main = do
    putStr (fst ("hello", "world"))
    putStr (snd ("hello", "world"))
