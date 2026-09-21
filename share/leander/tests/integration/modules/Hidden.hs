module Hidden (visible) where
-- secret is declared but not exported, so no importer may name it --
-- not even with a qualifier.
visible :: Int
visible = secret + 1

secret :: Int
secret = 41
