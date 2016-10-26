import Test.QuickCheck

qsort :: (Ord a) => [a] -> [a]  
qsort [] = []  
qsort (x:xs) =   
 let smallerSorted = qsort [a | a <- xs, a <= x]  
     biggerSorted = qsort [a | a <- xs, a > x]  
 in  smallerSorted ++ [x] ++ biggerSorted  

prop_idempotent xs = qsort (qsort xs) == qsort xs
prop_minimum xs = head (qsort xs) == minimum xs

main = do 
   quickCheck (prop_minimum :: [Integer] -> Bool)
   quickCheck (prop_idempotent :: [Integer] -> Bool)
