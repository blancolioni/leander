module test_51_selector_clash where

--  A field label that is also a top-level binding (#33).

data T51g = T51g { a51g :: Int }

a51g :: Int
a51g = 3
