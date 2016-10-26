import Data.List

pairsInLists :: (Eq a, Ord a) => [[a]] -> [[a]] -> Int -> Int -> Int -> [(a,a)]
pairsInLists l1 l2 n1 n2 gap = pairInList1 (group . sort $ findPairs l1 gap) (group . sort $ findPairs l2 gap) n1 n2

pairInList1 :: (Eq a, Ord a) => [[(a,a)]] -> [[(a,a)]] -> Int -> Int -> [(a,a)]
pairInList1 [] _ _ _ = []
pairInList1 (l1:ll1) l2 n1 n2
   | length l1 >= n1 = (pairInList2 (l1!!0) l2 n2) ++ pairInList1 ll1 l2 n1 n2
   | otherwise = pairInList1 ll1 l2 n1 n2


pairInList2 :: (Eq a, Ord a) => (a,a) -> [[(a,a)]] -> Int -> [(a,a)]
pairInList2 _ [] _ = []
pairInList2 pair (l:ll) n
   | pair `elem` l && length l <= n = [pair]
   | pair `elem` l && length l > n = []
   | otherwise = pairInList2 pair ll n

findPairs :: (Eq a, Ord a) => [[a]] -> Int -> [(a,a)]
findPairs [] _ = []
findPairs (l:ls) gap = nub (findPairsInList l gap) ++ findPairs ls gap

findPairsInList :: (Eq a, Ord a) => [a] -> Int -> [(a,a)]
findPairsInList [] _ = []
findPairsInList (e:es) gap
   | length es <= gap = []
   | otherwise = (e, es!!gap) : findPairsInList es gap