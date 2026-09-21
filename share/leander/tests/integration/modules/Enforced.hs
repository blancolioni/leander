module Enforced where
-- A qualified import brings nothing in unqualified, so "area" is not a
-- name this module may write -- only S.area is.
import qualified Shapes as S

enforcedArea :: Int
enforcedArea = S.area (S.Circle 2)
