module BadHiddenLabel where

--  An abstract type's labels are not exported with it (#33).

import Records

leak :: Int
leak = sealed (mkSealed 3)
