module Shapes (area, Shape(..), Unit(Unit)) where
-- An exporting module: an export list covering a value, a type with all
-- its constructors, and a type with one named constructor.
data Shape = Circle Int | Square Int

data Unit = Unit

area :: Shape -> Int
area (Circle r) = r * r * 3
area (Square s) = s * s
