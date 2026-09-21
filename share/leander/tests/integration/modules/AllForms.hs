module AllForms where
-- Every import shape the grammar accepts. The qualified, aliased and
-- selective ones are recorded but not yet enforced, so each still brings
-- its module in whole and unqualified -- which is why this module can
-- name tally despite hiding area.
import qualified Shapes as S
import Data.List (tally)
import Shapes hiding (area)

value :: Int
value = tally 3
