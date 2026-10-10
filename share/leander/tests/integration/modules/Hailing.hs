module Hailing (Hailing(..), hailWord) where
-- The same, with the helper exported.  Importing this used to compile the
-- default a second time, under the same name, and fail an assertion.

class Hailing a where
  hailName :: a -> String
  hail :: a -> String
  hail x = hailWord ++ hailName x

hailWord :: String
hailWord = "hi "

instance Hailing Bool where
  hailName _ = "bool"
