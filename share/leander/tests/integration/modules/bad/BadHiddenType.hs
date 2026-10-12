module BadHiddenType where
-- Abstract exports the constructor Pack, not the type Pack.
import Abstract
packed :: Pack -> Bool
packed _ = True
