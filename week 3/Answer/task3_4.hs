import Data.List
import Data.Ord(comparing)

pairWithMostOccurences :: (Eq a, Ord a) => [[a]] -> Int -> (a,a)
pairWithMostOccurences l gap = maximumBy (comparing length) (group . sort $ (findPairs l gap)) !! 0

findPairs :: (Eq a, Ord a) => [[a]] -> Int -> [(a,a)]
findPairs [] _ = []
findPairs (l:ls) gap = nub (findPairsInList l gap) ++ findPairs ls gap

findPairsInList :: (Eq a, Ord a) => [a] -> Int -> [(a,a)]
findPairsInList [] _ = []
findPairsInList (e:es) gap
   | length es <= gap = []
   | otherwise = (e, es!!gap) : findPairsInList es gap

-- still would need to call findPairsInList with gap values 0..