module UseAbstract where
-- Uses what Abstract exports, as Abstract means it to be used.
import Abstract

uaBoxed :: Box
uaBoxed = mkBox 41

uaValue :: Int
uaValue = unBox uaBoxed + 1

uaUnpack :: Wrap -> Int
uaUnpack (Pack n) = n

uaPacked = uaUnpack (Pack 7)
