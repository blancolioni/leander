module test_41_infix_lhs where

--  Every name here is prefixed: the integration suite shares one handle,
--  so a module's top-level names stay visible to later modules, and a
--  clash silently resolves to the earlier definition rather than failing.

--  Each operand of an infix definition is a whole pattern, and a
--  definition can be written applied inside parentheses (#132).

--  A constructor applied to a pattern, as the left operand.
(<||>) :: Maybe a -> Maybe a -> Maybe a
Just x <||> _ = Just x
Nothing <||> y = y

--  As the right operand: one pattern, not two.
(<+>) :: Maybe Int -> Maybe Int -> Int
(Just x) <+> Just y = x + y
_ <+> _ = 0

--  Behind a backticked name.
ilOrElse :: Maybe a -> Maybe a -> Maybe a
Just x `ilOrElse` _ = Just x
Nothing `ilOrElse` y = y

--  Both sides, with more than one argument each.
data IlPoint = IlPoint Int Int

(<.>) :: IlPoint -> IlPoint -> Int
IlPoint a b <.> IlPoint c d = a * c + b * d

--  "( funlhs ) apat": an operator definition, and a prefix one, applied
--  to a further argument.
(<**>) :: (Int -> Int) -> (Int -> Int) -> Int -> Int
(f <**> g) x = f (g x)

ilAdd3 :: Int -> Int -> Int -> Int
(ilAdd3 a b) c = a + b + c

ilLeft     = (Nothing <||> Just 3) == Just 3 && (Just 1 <||> Just 2) == Just 1
ilRight    = (Just 1 <+> Just 2) == 3 && (Just 1 <+> Nothing) == 0
ilBacktick = (Nothing `ilOrElse` Just 'x') == Just 'x'
ilBoth     = (IlPoint 1 2 <.> IlPoint 3 4) == 11
ilParen    = ((+ 1) <**> (* 2)) 5 == 11 && ilAdd3 1 2 3 == 6
