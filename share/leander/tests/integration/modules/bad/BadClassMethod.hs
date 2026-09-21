module BadClassMethod where
-- Classy declares "height" but does not export it.
import Classy
thing :: Int
thing = height True
