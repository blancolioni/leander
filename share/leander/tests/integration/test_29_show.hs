module test_29_show where

--  Every name here is prefixed: the integration suite shares one handle,
--  so a module's top-level names stay visible to later modules, and a
--  clash silently resolves to the earlier definition rather than failing.

--  Show as the Haskell 2010 Report has it: showsPrec, show and showList,
--  instances for the Prelude's types, and the ShowS helpers.  Each value
--  is True when show gives exactly what a Haskell implementation gives.

shInt         = show 42 == "42"
shNegative    = show (0 - 5) == "-5"
shNegativeArg = show (Just (0 - 5)) == "Just (-5)"
shNested      = show (Just (Just 3)) == "Just (Just 3)"
shList        = show [1, 2, 3] == "[1,2,3]"
shEmptyList   = show (tail [1]) == "[]"
shChar        = show 'a' == "'a'"
shQuoteChar   = show '\'' == "'\\''"
shNewline     = show '\n' == "'\\n'"
shString      = show "hello" == "\"hello\""
shEscapes     = show "say \"hi\"\n" == "\"say \\\"hi\\\"\\n\""
shStrings     = show ["ab", "c"] == "[\"ab\",\"c\"]"
shEither      = show [Left 1, Right True] == "[Left 1,Right True]"
shOrdering    = show (LT, EQ, GT) == "(LT,EQ,GT)"
shUnit        = show () == "()"
shMaybes      = show [Nothing, Just 'q'] == "[Nothing,Just 'q']"

--  A numeric escape followed by a digit, and \SO followed by H, need \&
--  to read back the same.
shProtected   = show "\200\&5\SO\&H\DEL\0" == "\"\\200\\&5\\SO\\&H\\DEL\\NUL\""

shPrecedence  = showsPrec 11 (0 - 7) "" == "(-7)"
shShows       = shows True (showString "!" "") == "True!"
shParen       = showParen True (showChar 'x') "" == "(x)"

--  A user instance that defines only show still gets showsPrec and
--  showList from the class.
data ShColour = ShRed | ShGreen

instance Show ShColour where
    show ShRed   = "ShRed"
    show ShGreen = "ShGreen"

shUserList    = show [ShRed, ShGreen] == "[ShRed,ShGreen]"
shUserMaybe   = show (Just ShGreen) == "Just ShGreen"
