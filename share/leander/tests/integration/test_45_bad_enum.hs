module test_45_bad_enum where

--  Enum is derivable only for an enumeration (#54).
data BadEnum = BadEnumA Int | BadEnumB deriving (Enum)
