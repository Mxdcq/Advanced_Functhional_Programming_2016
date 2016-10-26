import Control.Parallel (par, pseq)
import Control.DeepSeq (force)
import Data.List

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  
  
treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = singleton x  
treeInsert x (Node a left right)   
    | x == a = Node x left right  
    | x < a  = Node a (treeInsert x left) right  
    | x > a  = Node a left (treeInsert x right)

searchB s1 EmptyTree = []
searchB s1 (Node s leftTree rightTree)
  | allElem s1 s  = force searchBLeft `par` (searchBRight `pseq` (searchBLeft ++ [s] ++ searchBRight))
  | otherwise = force searchBLeft `par` (searchBRight `pseq` (searchBLeft ++ searchBRight))
  where searchBLeft = searchB s1 leftTree
        searchBRight = searchB s1 rightTree

t = treeInsert "hello" $ treeInsert "hill" $ treeInsert "house" $ treeInsert "apple" EmptyTree

allElem [] s2 = True
allElem (x:xs) s2 =
   if (x `elem` s2) then
      allElem xs s2
   else False
