module test_50_input where

--  getLine, readLn, and getChar at the end of input (#89).
main :: IO ()
main = do
  line <- getLine
  putStrLn (reverse line)
  n <- readLn
  print (n + length line)
  c <- catch getChar (\e -> putStrLn (show e) >> return '!')
  print c
