module BadHidingType where
-- Hiding Pair hides the type.
import Pair hiding (Pair)
total :: Pair -> Int
total p = pairSum p
