module Fixture where
-- A module that no other test has loaded, so that loading it really does
-- run a parse rather than coming back out of the module cache.
twice :: Int -> Int
twice n = n + n
