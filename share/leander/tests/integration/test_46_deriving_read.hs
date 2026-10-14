module test_46_deriving_read where

--  Every name here is prefixed: the integration suite shares one handle,
--  so a module's top-level names stay visible to later modules, and a
--  clash silently resolves to the earlier definition rather than failing.

--  Deriving Read reads what deriving Show writes (#154).

data DrColour = DrRed | DrGreen | DrBlue deriving (Eq, Show, Read)

data DrShape = DrCircle Int | DrRect Int Int | DrEmpty
  deriving (Eq, Show, Read)

data DrTree a = DrLeaf | DrNode (DrTree a) a (DrTree a)
  deriving (Eq, Show, Read)

drColours :: [DrColour]
drColours = read "[DrRed, DrBlue,DrGreen]"

drShape :: DrShape
drShape = read "DrRect 2 (-3)"

drParens :: DrShape
drParens = read " ( DrCircle 4 ) "

drNested :: Maybe DrShape
drNested = read "Just (DrRect 1 2)"

drTree :: DrTree Int
drTree = DrNode DrLeaf 1 (DrNode DrLeaf 2 DrLeaf)

drMissing :: [(DrShape, String)]
drMissing = reads "DrRect 1"

drUnparenthesised :: [(Maybe DrShape, String)]
drUnparenthesised = reads "Just DrRect 1 2"

drEnumeration = drColours == [DrRed, DrBlue, DrGreen]
drFields      = drShape == DrRect 2 (negate 3) && drParens == DrCircle 4
drPrecedence  = drNested == Just (DrRect 1 2) && null drUnparenthesised
drRoundTrip   = read (show drTree) == drTree
                && read (show [Just DrEmpty, Nothing])
                     == [Just DrEmpty, Nothing]
drNoParse     = null drMissing
