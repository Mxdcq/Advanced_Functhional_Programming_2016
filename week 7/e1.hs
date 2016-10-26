import Control.Parallel (par, pseq)
import Control.DeepSeq (force)
import Data.List

findPairs :: Int -> String -> String -> [(Char, Char)]
findPairs gap s1 s2
  | gap < 0   = error "Gap should be >= 0"
  | otherwise = force (formPairs s1) `par` ( force (formPairs s2) `pseq` (intersect (formPairs s1) (formPairs s2)))
  where
    formPairs :: String -> [(Char, Char)]
    formPairs s = formPairs' s (drop (gap + 1) s)

    formPairs' :: String -> String -> [(Char, Char)]
    formPairs' _ "" = []
    formPairs' (x:xs) (y:ys) = [(x, y)] ++ formPairs' xs ys


