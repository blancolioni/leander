module Pair (Pair(..), pairSum) where
-- A type and its constructor share the name Pair, and both are exported:
-- an import list chooses between them (#144).
data Pair = Pair Int Int

pairSum :: Pair -> Int
pairSum (Pair a b) = a + b
