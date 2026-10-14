module test_45_deriving where

--  Every name here is prefixed: the integration suite shares one handle,
--  so a module's top-level names stay visible to later modules, and a
--  clash silently resolves to the earlier definition rather than failing.

--  Deriving Ord, Show, Enum and Bounded (#54).

data DvColour = DvRed | DvGreen | DvBlue
  deriving (Eq, Ord, Show, Enum, Bounded)

data DvShape = DvCircle Int | DvRect Int Int | DvEmpty
  deriving (Eq, Ord, Show)

data DvPair a b = DvPair a b deriving (Eq, Ord, Show, Bounded)

data DvTree a = DvLeaf | DvNode (DvTree a) a (DvTree a)
  deriving (Eq, Ord, Show)

dvPairBound :: DvPair Bool DvColour
dvPairBound = minBound

dvColourList :: [DvColour]
dvColourList = [minBound .. maxBound]

--  Constructors compare in declaration order, fields left to right.
dvOrd = compare DvRed DvBlue == LT
        && compare (DvRect 1 2) (DvRect 1 3) == LT
        && compare DvEmpty (DvCircle 9) == GT
        && maximum [DvCircle 5, DvRect 0 0, DvCircle 7] == DvRect 0 0
        && DvNode DvLeaf 1 DvLeaf > DvLeaf

--  A constructor with fields is parenthesised inside another.
dvShow = show [DvRed, DvBlue] == "[DvRed,DvBlue]"
         && show (DvRect 2 (negate 3)) == "DvRect 2 (-3)"
         && show (Just (DvPair 'x' [DvGreen])) == "Just (DvPair 'x' [DvGreen])"
         && show (DvNode DvLeaf 1 (DvNode DvLeaf 2 DvLeaf))
              == "DvNode DvLeaf 1 (DvNode DvLeaf 2 DvLeaf)"

dvEnum = map fromEnum [DvRed, DvGreen, DvBlue] == [0, 1, 2]
         && toEnum 1 == DvGreen
         && succ DvRed == DvGreen && pred DvBlue == DvGreen
         && [DvGreen ..] == [DvGreen, DvBlue]
         && [DvBlue, DvGreen ..] == [DvBlue, DvGreen, DvRed]

dvBounded = dvColourList == [DvRed, DvGreen, DvBlue]
            && dvPairBound == DvPair False DvRed
            && maxBound == DvPair True DvBlue

--  The Prelude's own Ordering derives them too, and Int ranges count
--  down as well as up.
dvPrelude = [minBound .. maxBound] == [LT, EQ, GT]
            && compare LT GT == LT
            && [5, 4 .. 1] == [5, 4, 3, 2, 1]
            && take 3 [1, 3 ..] == [1, 3, 5]
            && [False ..] == [False, True]
