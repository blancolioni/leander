module UsePairQualifiedType where
-- Hiding Pair still leaves the type reachable behind the module's name.
import Pair hiding (Pair)

upqSum :: Pair.Pair -> Int
upqSum p = pairSum p

upqValue = upqSum (Pair.Pair 1 2) == 3
