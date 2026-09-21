module BadQualified where
-- area is only reachable as S.area; naming it bare must be rejected.
import qualified Shapes as S
thing :: Int
thing = area (S.Square 2)
