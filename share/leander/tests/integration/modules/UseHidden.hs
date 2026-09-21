module UseHidden where
-- Hidden exports only "visible". "secret" is reachable from inside
-- Hidden itself -- visible is secret + 1 -- and from nowhere else.
import Hidden

throughExport :: Int
throughExport = visible
