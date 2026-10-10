module test_35_imports_syntax_error where

--  Importing a module with a syntax error is reported, not a crash
--  (issue #113).

import test_35_imported_syntax_error

isUser = isGood + 1
