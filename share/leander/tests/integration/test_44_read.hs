module test_44_read where

--  Every name here is prefixed: the integration suite shares one handle,
--  so a module's top-level names stay visible to later modules, and a
--  clash silently resolves to the earlier definition rather than failing.

--  Read and its instances (#90).

rdInt :: Int
rdInt = read " (-17) "

rdInts :: [Int]
rdInts = read "[1, 2,3 ]"

rdChar :: Char
rdChar = read "'\n'"

rdString :: String
rdString = read "\"a\tb\\\"c\SOH\&H\65\""

rdMaybes :: [Maybe Int]
rdMaybes = read "[Nothing, Just 4, (Just (-5))]"

rdEither :: Either Int Bool
rdEither = read "Right False"

rdTuple :: (Int, String, Ordering, ())
rdTuple = read "(7, \"seven\", GT, ())"

rdReads :: [(Int, String)]
rdReads = reads "12 rest"


--  A type of the module's own, read through the class's readList default.
data RdColour = RdRed | RdGreen deriving (Eq)

instance Read RdColour where
    readsPrec _ s = do (tok, t) <- lex s
                       if tok == "RdRed" then [(RdRed, t)]
                       else if tok == "RdGreen" then [(RdGreen, t)]
                       else []

rdColours :: [RdColour]
rdColours = read "[RdGreen, RdRed]"

rdValue :: [(Maybe Char, [Either Int String])]
rdValue = [(Just 'x', [Left (negate 1), Right "two\n"])]

rdBasic    = rdInt == negate 17 && rdInts == [1, 2, 3] && rdChar == '\n'
rdStrings  = rdString == "a\tb\"c\SOH\&HA"
rdNested   = rdMaybes == [Nothing, Just 4, Just (negate 5)] && rdEither == Right False
rdTuples   = rdTuple == (7, "seven", GT, ())
rdPartial  = rdReads == [(12, " rest")]
rdLex      = map fst (lexAll rdLexedInput) == ["foo123", "'x'", "\"s\"", "<=>", "1.5e3", "rest"]
rdOwn      = rdColours == [RdGreen, RdRed]
rdRound    = read (show rdValue) == rdValue

rdLexedInput :: String
rdLexedInput = "  foo123 'x' \"s\" <=> 1.5e3 rest"

lexAll :: String -> [(String, String)]
lexAll s = case lex s of
             [(tok, rest)] -> if null tok then [] else (tok, rest) : lexAll rest
             _             -> []
