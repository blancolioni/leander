module test_49_show_function where

--  There is no Show instance for a function, and none can be found for
--  any type a function makes: reported where the binding is, not left
--  to crash when the program is linked (#76).
main :: IO ()
main = do
  putStrLn "never run"
  putStrLn (show map)
