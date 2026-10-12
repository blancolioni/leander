module BadHidingCon where
-- Hiding Pair hides its constructor too.
import Pair hiding (Pair)
made = pairSum (Pair 1 2)
