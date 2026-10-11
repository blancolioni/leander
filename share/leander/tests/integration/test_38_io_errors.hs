module test_38_io_errors where

--  Every name here is prefixed: the integration suite shares one handle,
--  so a module's top-level names stay visible to later modules, and a
--  clash silently resolves to the earlier definition rather than failing.

--  An IO action can fail with an IOError, and catch handles it (#88).

ieShow :: IOError -> IO [Char]
ieShow e = return (show e)

--  The handler gets the error, and nothing after the failure runs.
ieCaught = runIO (catch (ioError (userError "boom") >> return "after")
                        ieShow)
           == "user error (boom)"

--  A result computed before the failure reaches the error.
ieFlow = runIO (catch (return 41 >>= \x -> ioError (userError (show (x + 1))))
                      ieShow)
         == "user error (42)"

--  An action that does not fail passes its result through.
ieNoError = runIO (catch (return 5) (\e -> return 0)) == 5

--  A handler can fail in turn, for an outer catch.
ieNested = runIO (catch (catch (ioError (userError "one"))
                               (\e -> ioError (userError "two")))
                        ieShow)
           == "user error (two)"

--  A failure part way through mapM abandons the rest.
ieCheck :: Int -> IO Int
ieCheck x = if x == 3 then ioError (userError "three") else return x

ieMapM = runIO (catch (mapM ieCheck [1, 2, 3, 4]) (\e -> return [])) == []
         && runIO (mapM ieCheck [1, 2]) == [1, 2]

ieEq = userError "a" == userError "a" && not (userError "a" == userError "b")

--  An action run for its effects still runs them before its handler.
ieEffects :: IO Bool
ieEffects = do
    r <- catch (putStr "" >> ioError (userError "x") >> return False)
               (\e -> return True)
    return r

ieDo = runIO ieEffects
