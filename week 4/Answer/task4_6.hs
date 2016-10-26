import Data.List
import Data.Ord(comparing)

-- please read readme_4_6.txt

-- find out the support of each pair with gap constraint
pairSupport ls gap n =
 filter (\(k,l) -> if k<n then False else True) $
  map (\l -> (length l, head l)) $  (group. sort . concat) $ map (nub . project2)  $  findPairs ls gap

project2 [] = []
project2 ((x,y,_,_):xs) = (x,y): (project2 xs)

findPairs :: (Eq a, Ord a) => [[a]] -> Int -> [[(a,a,Int,Int)]]
findPairs [] _ = []
findPairs (l:ls) gap = (findPairsInList l l gap 0) : findPairs ls gap

findPairsInList :: (Eq a, Ord a) => [a] -> [a] -> Int -> Int -> [(a,a,Int,Int)]
findPairsInList [] l 0 _ = []
findPairsInList [] l g _ = findPairsInList l l (g-1) 0
findPairsInList (e:es) l gap pos
   | length es <= gap = findPairsInList [] l gap 0
   | otherwise = (e, es!!gap, gap, pos) : findPairsInList es l gap (pos+1)

