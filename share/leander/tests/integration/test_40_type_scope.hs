module test_40_type_scope where

--  Every name here is prefixed: the integration suite shares one handle,
--  so a module's top-level names stay visible to later modules, and a
--  clash silently resolves to the earlier definition rather than failing.

--  A type may be named before the declaration that makes it (#142).

tsArea :: TsShape -> Int
tsArea (TsSquare n) = n * n

tsLabel :: TsLabel
tsLabel = "square"

instance Show TsShape where
    show (TsSquare n) = "TsSquare " ++ show n

data TsShape = TsSquare Int

type TsLabel = [Char]

tsForward = tsArea (TsSquare 4) == 16 && show (TsSquare 2) == "TsSquare 2"
            && tsLabel == "square"
