--We say that character pair (c1,c2) appear in string s with gap g, if c1 is before c2 and there are exactly g characters between c1 and c2 in s.  
--Write a function that, given a gap g and strings s1 and s2, finds all character pairs that appear with gap g in both s1 and s2. 
--E.g. if gap = 2 and s1 ="afcfa" and s2="aaaaafff", then there is just one such character pair: ('a','f')

import Data.List
import Control.Parallel (par, pseq)

-- task 2.5
findPairs :: Int -> String -> String -> [(Char, Char)]
findPairs gap s1 s2
  | gap < 0   = error "Gap should be >= 0"
  | length s1 < length s2 = (formPairs s1) `par` ((formPairs s2) `pseq` (intersect (formPairs s1) (formPairs s2)))
  | otherwise = (formPairs s2) `par` ((formPairs s1) `pseq` (intersect (formPairs s2) (formPairs s1)))
  where
    formPairs :: String -> [(Char, Char)]
    formPairs s = formPairs' s (drop (gap + 1) s)

    formPairs' :: String -> String -> [(Char, Char)]
    formPairs' _ "" = []
    formPairs' (x:xs) (y:ys) = [(x, y)] ++ formPairs' xs ys 