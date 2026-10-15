module Records (RecPoint(..), Sealed, mkSealed, Partial(P)) where

--  Field labels across modules (#33): T(..) brings a type's labels with
--  its constructors, and naming only some constructors does not.

data RecPoint = RecPoint { recX, recY :: Int }

data Sealed = Sealed { sealed :: Int }

data Partial = P { shown :: Int } | Q { other :: Int }

mkSealed :: Int -> Sealed
mkSealed n = Sealed { sealed = n }
