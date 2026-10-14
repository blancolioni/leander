module test_49_missing_instance where

--  A missing instance in one binding is reported against that binding,
--  and the bindings around it are not blamed for it (#157).
data NoEqHere = NoEqHere

miFirst = 1 + 1 == 2

miSecond = 'a' == 'a'

miThird = (NoEqHere, 'b') == (NoEqHere, 'b')
