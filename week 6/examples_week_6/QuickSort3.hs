-- import Test.QuickCheck
import Debug.QuickCheck.Batch
import Data.List
import Test.QuickCheck.State
import Test.QuickCheck.Gen
import Test.QuickCheck.All

qsort :: (Ord a) => [a] -> [a]  
qsort [] = []  
qsort (x:xs) =   
 let smallerSorted = qsort [a | a <- xs, a <= x]  
     biggerSorted = qsort [a | a <- xs, a > x]  
 in  smallerSorted ++ [x] ++ biggerSorted  

prop_idempotent xs = qsort (qsort xs) == qsort xs
prop_minimum xs = head (qsort xs) == minimum xs

options = TestOptions
  { no_of_tests = 200
  , length_of_tests = 1  -- time limit
  , debug_tests = False }

main = do
  runTests "simple" options
    [ run prop_idempotent
    , run prop_minimum
    ]
