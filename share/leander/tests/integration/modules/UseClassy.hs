module UseClassy where
-- Labelled(..) in an import list has to expand to the class's methods,
-- the same way it expands to a data type's constructors.
import Classy (Labelled(..), Sized(width))

viaWildcard :: Int
viaWildcard = label True + mark True

viaNamedMethod :: Int
viaNamedMethod = width True
