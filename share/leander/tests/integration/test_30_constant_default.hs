module test_30_constant_default where

--  Every name here is prefixed: the integration suite shares one handle,
--  so a module's top-level names stay visible to later modules, and a
--  clash silently resolves to the earlier definition rather than failing.

--  A default that uses no method of its own class.  An instance that
--  omits the method used to be told it had no such method (issue #126).

class CdLabel a where
    cdLabel :: a -> String
    cdLabel _ = "unlabelled"

instance CdLabel Bool where

data CdBox = CdBox Int

instance CdLabel CdBox where
    cdLabel (CdBox n) = "box"

cdDefault    = cdLabel True == "unlabelled"
cdOverridden = cdLabel (CdBox 1) == "box"

--  The same, beside a method the instance does define, declared after it.

class CdSized a where
    cdName :: a -> String
    cdSize :: a -> Int
    cdName _ = "thing"

instance CdSized Bool where
    cdSize _ = 1

cdBesideDefined = cdName False == "thing" && cdSize True == 1

--  A default that needs only a superclass, not its own class.

class Eq a => CdSame a where
    cdSame :: a -> a -> String
    cdSame x y = if x == y then "same" else "different"

instance CdSame Bool where

cdSuperclass = cdSame True True == "same" && cdSame True False == "different"
