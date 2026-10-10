module test_28_match_failure where

--  Matches only a non-empty list, so [] has no alternative, and evaluating
--  mfFirst [] must report where and in which function (issue #116).

mfFirst (x:_) = x
