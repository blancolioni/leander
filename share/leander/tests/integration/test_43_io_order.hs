module test_43_io_order where

--  What a program writes before a runtime error is written: an action's
--  effects happen when it runs, not when runIO forces the last world
--  token at the end (#151).

ioEmpty :: [Int]
ioEmpty = []

main :: IO ()
main = do
  putStrLn "before"
  print (head ioEmpty)
