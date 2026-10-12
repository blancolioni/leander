module UsePairType where
-- "Pair" alone in an import list is the type: the constructor is still
-- there behind the module's name, but not unqualified.
import Pair (Pair, pairSum)

uptSum :: Pair -> Int
uptSum p = pairSum p

uptValue = uptSum (Pair.Pair 3 4) == 7
