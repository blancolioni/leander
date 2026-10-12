module BadUnknownInstance where
-- An instance for a type that does not exist.
instance Eq Phantom where
    a == b = True
