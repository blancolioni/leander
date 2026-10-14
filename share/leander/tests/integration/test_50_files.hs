module test_50_files where

--  writeFile, appendFile and readFile, and a file that is not there (#89).
--  The integration suite deletes the file afterwards.
path :: FilePath
path = "leander_test_50_files.txt"

main :: IO ()
main = do
  writeFile path "one\n"
  appendFile path "two\n"
  s <- readFile path
  putStr s
  print (length (lines s))
  r <- catch (readFile "no/such/leander_file.txt") (\e -> return (show e))
  putStrLn r
