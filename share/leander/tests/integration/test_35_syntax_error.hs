module test_35_syntax_error where

--  A syntax error partway through the declarations is reported, and the
--  module fails to load; it must not crash (issue #113).

seGood :: Int
seGood = 1

seBad x = )

seAfter = 2
