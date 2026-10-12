module Abstract (Box, box, mkBox, unBox, Wrap(..)) where
-- Box is exported without its constructor, which shares its name: an
-- importer may name the type, but cannot build or match a Box itself
-- (#140). Nor does exporting the function box, a name that differs only
-- in case, export the constructor. Wrap's constructor Pack shares its
-- name with a type that is not exported, Pack: an importer may use the
-- constructor, not the type.
data Box = Box Int

box :: Int -> Box
box n = Box n

mkBox :: Int -> Box
mkBox n = box n

unBox :: Box -> Int
unBox (Box n) = n

data Pack = Packed

data Wrap = Pack Int
