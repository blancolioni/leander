module Classy (Labelled(..), Sized(width)) where
-- Two ways of exporting a class's methods: "(..)" for all of them, and a
-- named list for some. "height" is declared but not exported.
class Labelled a where
  label :: a -> Int
  mark :: a -> Int

class Sized a where
  width :: a -> Int
  height :: a -> Int

instance Labelled Bool where
  label b = 3
  mark b = 4

instance Sized Bool where
  width b = 5
  height b = 6
