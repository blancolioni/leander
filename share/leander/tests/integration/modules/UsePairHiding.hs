module UsePairHiding where
-- Hiding Pair hides the type and the constructor of that name, and
-- nothing else.
import Pair hiding (Pair)

uphValue = pairSum (Pair.Pair 5 6) == 11
