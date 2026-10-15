module test_51_pattern_field where

--  A record pattern naming a field the constructor lacks (#33).

data T51h = T51h { a51h :: Int }

x51h :: T51h -> Int
x51h T51h { c51h = y } = 1
