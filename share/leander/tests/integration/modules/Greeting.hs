module Greeting (Greeting(..)) where
-- A class whose default method uses a helper the module does not export.
-- The default is compiled here, where the helper is in scope, and an
-- importing module reaches it by linkage (issue #122).

class Greeting a where
  greetName :: a -> String
  greet :: a -> String
  greet x = greetingWord ++ greetName x

greetingWord :: String
greetingWord = "hello "

instance Greeting Bool where
  greetName _ = "bool"
