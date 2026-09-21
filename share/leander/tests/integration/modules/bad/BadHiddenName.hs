module BadHiddenName where
-- Hidden does not export secret.
import Hidden
thing :: Int
thing = secret
