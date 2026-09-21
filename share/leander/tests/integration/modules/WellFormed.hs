module WellFormed where
-- The control for the rejection tests: a module that uses every construct
-- they get wrong, correctly. If this ever reports an error, those tests
-- are passing on sticky state rather than on their own diagnostic.
import Shapes (area, Shape(..))
import qualified Data.List as L

wellFormedArea :: Int
wellFormedArea = area (Square 3) + L.tally 1
