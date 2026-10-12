module test_39_case where

--  Every name here is prefixed: the integration suite shares one handle,
--  so a module's top-level names stay visible to later modules, and a
--  clash silently resolves to the earlier definition rather than failing.

--  Names that differ only in case are different names (#143).

csIsEof :: Int
csIsEof = 1

csIsEOF :: Int
csIsEOF = 2

data CsBox = CsBox Int
data CSBOX = CSBOX Int Int

csUnbox (CsBox n) = n
csUnBOX (CSBOX a b) = a + b

class CsShape a where
    csArea :: a -> Int

class CSSHAPE a where
    csAREA :: a -> Int

instance CsShape CsBox where
    csArea (CsBox n) = n * n

instance CSSHAPE CsBox where
    csAREA (CsBox n) = n + n

csValues   = csIsEof == 1 && csIsEOF == 2
csTypes    = csUnbox (CsBox 3) == 3 && csUnBOX (CSBOX 3 4) == 7
csClasses  = csArea (CsBox 5) == 25 && csAREA (CsBox 5) == 10
