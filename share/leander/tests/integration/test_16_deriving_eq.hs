module test_16_deriving_eq where

data Color = Red | Green | Blue deriving Eq

data Pair = P Int Int deriving Eq

data Mix = L | R Int deriving Eq

data MyList = MNil | MCons Int MyList deriving Eq

colorEq = Red == Red
colorNe = Red == Green
colorBl = Blue /= Red
pairEq  = P 1 2 == P 1 2
pairNe1 = P 1 2 == P 1 3
pairNe2 = P 1 2 == P 2 2
mixLL   = L == L
mixRR   = R 5 == R 5
mixLR   = L == R 7
mixRdif = R 1 == R 2
recEq   = MCons 1 (MCons 2 MNil) == MCons 1 (MCons 2 MNil)
recNe   = MCons 1 (MCons 2 MNil) == MCons 1 (MCons 3 MNil)
recNil  = MCons 1 MNil == MCons 1 MNil
