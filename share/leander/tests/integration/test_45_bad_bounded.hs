module test_45_bad_bounded where

--  Bounded needs an enumeration, or a single constructor (#54).
data BadBounded = BadBoundedA Int | BadBoundedB deriving (Bounded)
