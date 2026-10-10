module test_20_guards where

--  Every name here is prefixed: the integration suite shares one handle,
--  so a module's top-level names stay visible to later modules, and a
--  clash silently resolves to the earlier definition rather than failing.

--  A guard set that ends in 'otherwise' can never fall out of its
--  equation, so the equation keeps its shape.

gdSign :: Int -> Int
gdSign x | x > 0     = 1
         | x < 0     = 0 - 1
         | otherwise = 0

gdPosOne = gdSign 7
gdNegOne = gdSign (0 - 7)
gdZero   = gdSign 0

--  Every guard failing has to continue at the next equation.  The
--  alternative has already been committed to by then, so this is the
--  case that needs the equation group staged.

gdBucket :: Int -> Int
gdBucket n | n > 100 = 1
gdBucket 0 = 2
gdBucket _ = 3

gdBig    = gdBucket 500
gdZeroed = gdBucket 0
gdSmall  = gdBucket 4

--  Falling out through a constructor pattern, not just a literal one.

gdFromJust :: Maybe Int -> Int
gdFromJust (Just x) | x > 0 = x
gdFromJust _ = 0

gdSomeBig   = gdFromJust (Just 9)
gdSomeSmall = gdFromJust (Just (0 - 9))
gdNoneAtAll = gdFromJust Nothing

--  Comma-separated guards conjoin.

gdBetween :: Int -> Int
gdBetween x | x > 3, x < 10 = 1
gdBetween _ = 0

gdInRange  = gdBetween 5
gdTooSmall = gdBetween 1
gdTooBig   = gdBetween 20

--  A guarded function that recurses.  The staged group is rebuilt as a
--  single equation, and if that equation lost its parameters it would
--  lose its Y wrap with them and stop resolving its own name.

gdCountDown :: Int -> Int
gdCountDown n | n > 0 = gdCountDown (n - 1)
gdCountDown _ = 42

gdCounted = gdCountDown 5

--  Guards on a multi-argument function, which is matched a column at a
--  time rather than in one dispatch.

gdGrade :: Int -> Int -> Int
gdGrade x y | x > y = 1
gdGrade 0 y = 2
gdGrade _ _ = 3

gdFirstBigger = gdGrade 9 1
gdFirstZero   = gdGrade 0 5
gdNeither     = gdGrade 1 5

--  Guards on case alternatives.  A case is desugared into equations, so
--  this is the same machinery reached by a different route.

gdClassify :: Int -> Int
gdClassify n =
  case n of
    m | m > 10 -> 1
      | m > 5  -> 2
    0 -> 3
    _ -> 4

gdOver10 = gdClassify 50
gdOver5  = gdClassify 7
gdIsZero = gdClassify 0
gdOther  = gdClassify 2

--  A guard can raise a class constraint of its own.  The synthesised
--  groups are monomorphic so that the constraint travels out to the
--  enclosing binding rather than being stranded (issue #70).

gdFirstWins :: Ord a => a -> a -> Int
gdFirstWins a b | a > b = 1
gdFirstWins _ _ = 0

gdOrdBigger  = gdFirstWins 8 3
gdOrdSmaller = gdFirstWins 3 8
gdOrdChar    = gdFirstWins 'z' 'a'

--  Returning the parameters themselves ties the guard's type to both of
--  them inside the synthesised groups, which unify straight into the
--  inference context.  That used to collide with the substitution the
--  enclosing application brought back, and the collision dropped the
--  link that resolves the guard's Ord dictionary -- even at Int
--  (issue #93).

gdMax :: Int -> Int -> Int
gdMax a b | a > b     = a
          | otherwise = b

gdMaxFirst  = gdMax 7 2
gdMaxSecond = gdMax 2 7

--  Falling out to an equation that binds the parameters again.

gdPick :: Int -> Int -> Int
gdPick a b | a > b = 1
gdPick a b = b

gdPickGuard = gdPick 3 1
gdPickFall  = gdPick 1 5

--  The same with the result type the constrained variable itself.

gdMaxOrd :: Ord a => a -> a -> a
gdMaxOrd a b | a > b     = a
             | otherwise = b

gdMaxOrdInt  = gdMaxOrd 4 9
gdMaxOrdChar = gdMaxOrd 'z' 'a'

--  A 'where' binds over the guards as well as over the bodies.

gdStepped :: Int -> Int
gdStepped x | doubled > 10 = doubled
            | otherwise    = 0
  where doubled = x + x

gdSteppedBig   = gdStepped 9
gdSteppedSmall = gdStepped 1
