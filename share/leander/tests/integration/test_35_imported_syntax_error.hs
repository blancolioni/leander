module test_35_imported_syntax_error where

--  Imported by test_35_imports_syntax_error, and by nothing else: the
--  module cache keeps a module that parsed with errors, so a second test
--  loading it would not see the error again.

isGood :: Int
isGood = 1

isBad x = )
