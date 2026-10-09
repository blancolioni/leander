module test_22_unterminated_char where

--  A character literal cut off by the end of the line has to be reported,
--  not read past: the lexer used to ask for the character after it.

ucBad = 'a
ucGood = 1
