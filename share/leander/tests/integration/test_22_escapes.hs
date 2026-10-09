module test_22_escapes where

--  Every name here is prefixed: the integration suite shares one handle,
--  so a module's top-level names stay visible to later modules.

esCodes :: [Char] -> [Int]
esCodes = map fromEnum

--  The single-character escapes.

esSimple = esCodes "\a\b\f\n\r\t\v\\\"\'" == [7,8,12,10,13,9,11,92,34,39]

--  Numeric escapes, in each base.

esNumeric = esCodes "\65\x41\x4a\o101" == [65,65,74,65]

--  ASCII control names take the longest match: \SOH, not \SO then H.

esNames = esCodes "\NUL\SOH\SO\DEL\SP" == [0,1,14,127,32]

--  \& is empty, which is what lets it end an escape early.

esEmpty = esCodes "\SO\&H\1\&2" == [14,72,1,50]

esCaret = esCodes "\^@\^A\^Z\^[\^_" == [0,1,26,27,31]

--  \^ takes exactly one character, even a backslash before the quote.

esCaretBackslash = esCodes "\^\" == [28]

--  A gap, on one line and across two.

esGap = "ab\    \cd" == "abcd"

esLongGap = "ab\
            \cd" == "abcd"

--  Character literals use the same escapes, quotes included.

esChar = esCodes ['\65', '\x42', '\'', '\\', '\n', '\DEL'] == [65,66,39,92,10,127]

esQuotes = fromEnum '\'' + fromEnum '"'
