import Data.List

-- Task 2.3
--Write a function that take as parameters:
--- a function f that, given two Int coordinate points, calculates their distance,
--- an Int coordinate point p, and
--- an distance d (also Int),
--calculates a list of such points that using the distance function f, the points' distance from the parameter point p is at most d.
--Your function's type should be:
--((Int,Int) -> (Int,Int) -> Int) -> (Int,Int) -> Int -> [(Int,Int)]    
--After that, use partial evaluation to create another function, where f has been fixed to the Manhattan distance function, by using partial evaluation. 
--Do that by fixing f with a lambda funciton. Manahttan distance is the x-distance + y-distance between the points. 
closePoints :: ((Int,Int) -> (Int,Int) -> Int) -> (Int,Int) -> Int -> [(Int,Int)]

closePoints f (x1,y1) d = 
  [(x,y) | x<-[(x1 - maxd)..(x1 + maxd)], y<-[(y1 - maxd)..(y1 + maxd)], f (x,y) (x1,y1) <= d]
  where maxd = findMax f (x1,y1) d 0

findMax f (x1,y1) d maxd 
  | f (x1,y1) (x1+maxd, y1) >= d = maxd
  | otherwise = findMax f (x1,y1) d (maxd+1) 
   
manh :: (Int,Int) -> (Int,Int) -> Int
manh (x1,y1) (x2,y2) = (abs (x1-x2)) + (abs (y1-y2))

-- closeByManh = closePoints manh
closeByManh = closePoints (\(x1,y1) (x2,y2) -> (abs (x1-x2)) + (abs (y1-y2)))


-- Task 5 & 6: one way

commonPairsWithG s1 s2 g = findCommon (pairsWithGap s1 g) (pairsWithGap s2 g)

pairsWithGap [] _ = []
pairsWithGap s g = findRemainingPairsWithG s g []

findRemainingPairsWithG [] _ ps = ps
findRemainingPairsWithG _ 0 ps = ps
findRemainingPairsWithG (x:xs) g ps
  | length xs <= g = []
  | (x, y) `elem` ps = findRemainingPairsWithG xs g ps
  | otherwise = (x,y) : findRemainingPairsWithG xs g ((x,y):ps)
  where y = xs !! g

findCommon s1 s2 = [x | x<-s1, x `elem` s2]

noPairsWithGap g s = length (pairsWithGap s g)

pairsWithGapsWithNos ss g = 
  map (\x -> (noPairsWithGap g x, x)) ss

quickAscSort [] = []
quickAscSort (x:xs) =
  let biggerSorted = quickAscSort (filter (>=x) xs)  
      smallerSorted = quickAscSort (filter (<x) xs)
  in biggerSorted ++ [x] ++ smallerSorted

-- task 2.5


findPairs :: Int -> String -> String -> [(Char, Char)]
findPairs gap s1 s2
  | gap < 0   = error "Gap should be >= 0"
  | otherwise = intersect (formPairs s1) (formPairs s2)
  where
    formPairs :: String -> [(Char, Char)]
    formPairs s = formPairs' s (drop (gap + 1) s)

    formPairs' :: String -> String -> [(Char, Char)]
    formPairs' _ "" = []
    formPairs' (x:xs) (y:ys) = [(x, y)] ++ formPairs' xs ys

