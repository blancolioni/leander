module test_43_io_catch_order where

--  Effects stay in order around a caught IOError: what ran before the
--  failure comes before the handler's own output (#151).

main :: IO ()
main = do
  putStr "a"
  catch (putStr "b" >> ioError (userError "x") >> putStr "never")
        (\e -> putStr "c")
  putStrLn "d"
