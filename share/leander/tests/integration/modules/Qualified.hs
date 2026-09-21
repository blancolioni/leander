module Qualified where
-- Qualified names: a type in a signature, a function, and a data
-- constructor, all reached through the alias the import gave the module.
--
-- The names here are deliberately unusual. Test_Module shares one handle
-- across every fixture and top-level names are still stored bare, so an
-- ordinary name like "sized" resolves to whichever module defined it
-- first -- test_19_type_synonym.hs, as it happens. Canonical per-module
-- keys are what will fix that.
import qualified Shapes as S

qualShapeSize :: S.Shape -> Int
qualShapeSize s = S.area s

qualSquareArea :: Int
qualSquareArea = qualShapeSize (S.Square 5)

qualCircleArea :: Int
qualCircleArea = S.area (S.Circle 3)
