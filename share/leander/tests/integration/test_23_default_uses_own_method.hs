module test_23_default_uses_own_method where

--  Every name here is prefixed: the integration suite shares one handle,
--  so a module's top-level names stay visible to later modules.

--  A class default that uses the method it defines means that method at
--  whatever type it is used at, chosen by dictionary. It used to be tied
--  to itself instead, so it looped at the instance's own type (#94).

class DmSized a where
  dmSize   :: a -> Int
  dmDouble :: a -> Int
  dmDouble x = dmDouble (dmSize x)

instance DmSized Int where
  dmSize n = n
  dmDouble n = n * 2

instance DmSized Bool where
  dmSize b = if b then 10 else 20

dmDoubled = dmDouble True

--  Enum's own default enumFromTo is that shape too: it is
--  map toEnum [fromEnum x .. fromEnum y], and the range is enumFromTo at
--  Int.

data DmColor = DmRed | DmGreen | DmBlue

instance Enum DmColor where
  toEnum 0 = DmRed
  toEnum 1 = DmGreen
  toEnum _ = DmBlue
  fromEnum DmRed = 0
  fromEnum DmGreen = 1
  fromEnum DmBlue = 2

dmColors = map fromEnum [DmRed .. DmBlue]

dmLetters = ['a' .. 'e']
