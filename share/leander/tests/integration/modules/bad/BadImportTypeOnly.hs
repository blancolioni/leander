module BadImportTypeOnly where
-- Pair alone imports the type, not the constructor.
import Pair (Pair, pairSum)
made = pairSum (Pair 1 2)
