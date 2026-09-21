module Shadowing where
-- A module's own declaration wins over one it imported under the same
-- name, and the imported one is still reachable with a qualifier: the
-- two live under different keys once the import has renamed Tagged's
-- own names to its own.
import Tagged

tagValue :: Int
tagValue = 2

mine :: Int
mine = tagValue

theirs :: Int
theirs = Tagged.tagValue
