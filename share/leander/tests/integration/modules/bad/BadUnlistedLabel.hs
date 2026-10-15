module BadUnlistedLabel where

--  Exporting P alone does not export the labels of P (#33).

import Records

leak :: Int
leak = shown (P 1)
