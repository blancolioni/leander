module BadReExport (module Data.List, thing) where
thing :: Int
thing = 1
