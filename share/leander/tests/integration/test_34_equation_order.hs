module test_34_equation_order where

--  Every name here is prefixed: the integration suite shares one handle,
--  so a module's top-level names stay visible to later modules, and a
--  clash silently resolves to the earlier definition rather than failing.

--  Equations are tried in order, each falling through to the next when
--  any of its patterns fails (issue #120).

--  Two equations with the same first constructor, the first failing in a
--  later column: the Prelude's (!!) has this shape.
eoIndex = [10, 20, 30] !! 0 == 10 && [10, 20, 30] !! 2 == 30

eoNth :: [Int] -> Int -> Int
eoNth (x:_)  0 = x
eoNth (_:xs) n = eoNth xs (n - 1)

eoNthLater = eoNth [10, 20, 30] 1 == 20

--  A constructor equation failing in a later column, before an equation
--  that starts with a variable.
data EoColour = EoRed | EoGreen

eoSame :: EoColour -> EoColour -> Bool
eoSame EoRed   EoRed   = True
eoSame EoGreen EoGreen = True
eoSame _       _       = False

eoFallThrough = eoSame EoRed EoRed && not (eoSame EoRed EoGreen)

--  An equation that starts with a variable comes first, so it is tried
--  first, even for values a later constructor equation also matches.
eoFirst :: Maybe Int -> Int -> Int
eoFirst _        0 = 1
eoFirst (Just x) n = x
eoFirst Nothing  n = n

eoVariableFirst = eoFirst (Just 5) 0 == 1 && eoFirst (Just 5) 9 == 5

--  The same first constructor -- a pair -- told apart by a later column.
eoPair :: (Int, Int) -> [Int] -> Int
eoPair (a, b) []    = a
eoPair (a, b) (x:_) = b + x

eoSameConstructor = eoPair (1, 2) [] == 1 && eoPair (1, 2) [10] == 12

--  The same, with the variables further in.
eoDeep :: Int -> Maybe Int -> Int -> Int
eoDeep _ (Just x) 0 = x
eoDeep _ (Just x) n = n
eoDeep k Nothing  _ = k

eoDeeper = eoDeep 1 (Just 7) 0 == 7 && eoDeep 1 (Just 7) 3 == 3
             && eoDeep 4 Nothing 9 == 4
