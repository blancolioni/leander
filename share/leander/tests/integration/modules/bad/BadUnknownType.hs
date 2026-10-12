module BadUnknownType where
-- No type Nonexistent is declared or imported.
packed :: Nonexistent -> Bool
packed _ = True
