module BadAbstractCon where
-- Abstract exports the type Box, not its constructor Box.
import Abstract
open :: Box -> Int
open (Box n) = n
