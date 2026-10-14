module test_50_interact where

--  interact reads its input as its function needs it, so each character
--  of output follows the one read that made it (#89).
main :: IO ()
main = interact (map (\c -> if c == '\n' then c else succ c))
