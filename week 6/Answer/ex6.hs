import Data.Monoid
import Data.List (sort)
import Control.Applicative
import System.Environment
import Control.Concurrent
import Test.QuickCheck
import Control.Monad.Writer
import Control.Monad.Writer.Lazy

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)


-- ex 6.1

startCalc ::  Writer [String] [Int]
startCalc = do
       tell ["Started"]
       return []
logCalc :: [Int] -> String -> Writer [String] [Int]
logCalc (x:y:ys) "*" = do 
        tell ["Got: " ++ "* ;"] 
        return ((x*y):ys) 
logCalc (x:y:ys) "+" = do 
        tell ["Got: " ++ "* ;"] 
        return ((x+y):ys) 
logCalc (x:y:ys) "-" = do 
        tell ["Got: " ++ "* ;"] 
        return ((x-y):ys) 
logCalc ys s = do 
        tell ["Got: " ++ s ++ " ;"]
        return (read s: ys)

tryCalc = do
   a <- startCalc
   b <- logCalc a "3"
   c <- logCalc b "4"
   d <- logCalc c "*"
   return d

-- ex 6.2

readArgs :: [String] -> (Int,Int,Int,Int)
readArgs (a:b:c:d:_) = (read a,read b,read c,read d)
readArgs _ = (0,0,0,0)

main6_2 = do
    args <- getArgs
    let (a,b,c,d) = readArgs args
    m1 <- newMVar a
    m2 <- newMVar b
    m3 <- newEmptyMVar
    m4 <- newMVar c
    m5 <- newMVar d
    m6 <- newEmptyMVar
    forkIO $ do
       a1 <- takeMVar m1
       b1 <- takeMVar m2
       putMVar m3 (a1+b1)
    forkIO $ do
       a2 <- takeMVar m4
       b2 <- takeMVar m5
       putMVar m6 (a2+b2)
    sum1 <- takeMVar m3
    sum2 <- takeMVar m6
    print (sum1+sum2)


-- task 6.3

reverseNoZeros ::  [Int] -> [Int]
reverseNoZeros [] = []
reverseNoZeros (0:xs) = reverseNoZeros xs
reverseNoZeros (x:xs) = reverseNoZeros xs ++ [x]

prop_idempotent2 xs = reverseNoZeros (reverseNoZeros (reverseNoZeros xs)) == reverseNoZeros xs
-- prop_nonzero_head xs = head (reverseNoZeros xs) /= 0
prop_nonzero_head xs 
  | xs==[] = True
  | otherwise = head (reverseNoZeros xs) /= 0



-- ex 6.4



-- instance Monad Tree where
--  return x = Node x EmptyTree EmptyTree
--  EmptyTree >>= f = EmptyTree
--  Node x y z >>= f = Node (f x) (f y) (f z)


singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  
  
treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = singleton x  
treeInsert x (Node a left right)   
    | x == a = Node x left right  
    | x < a  = Node a (treeInsert x left) right  
    | x > a  = Node a left (treeInsert x right)


searchA s1 s2 EmptyTree = []
searchA s1 s2 (Node s leftTree rightTree)
  | (s <= s2) && (s1 <= s) = (searchA s1 s2 leftTree) ++ [s] ++ (searchA s1 s2 rightTree)
  | otherwise = (searchA s1 s2 leftTree) ++ (searchA s1 s2 rightTree)

searchB s1 EmptyTree = []
searchB s1 (Node s leftTree rightTree)
  | allElem s1 s  = (searchB s1 leftTree) ++ [s] ++ (searchB s1 rightTree)
  | otherwise = (searchB s1 leftTree) ++ (searchB s1 rightTree)

t = treeInsert "hello" $ treeInsert "hill" $ treeInsert "house" $ treeInsert "apple" EmptyTree

allElem [] s2 = True
allElem (x:xs) s2 =
   if (x `elem` s2) then
      allElem xs s2
   else False



-- ex 6.5

-- Note, this can be very slow and the idea maybe not so
-- good, but I wanted to follow the book's chess example

elemInd e xs = elemIndc e xs 0
elemIndc _ [] _ = error "not an element"
elemIndc e (x:xs) i 
  | e == x = i
  | otherwise = elemIndc e xs (i+1)

row1 = "qwertyuiop"
row2 = "asdfghjkl"
row3 = "zxcvbnm"

neighbours c s 
  | pos == 0 = c:(s!!1):[]
  | pos < (length s - 1) = (s !! (pos-1)):c:(s!!(pos+1)):[]
  | otherwise = (s !! (pos-1)):c:[]
  where pos = elemInd c s

edit c 
 | c `elem` row1 = neighbours c row1
 | c `elem` row2 = neighbours c row2
 | c `elem` row3 = neighbours c row3 
 | otherwise = [c]

changeOneNeighbour s = changeNeighbours [] s []

changeNeighbours start [] xs = xs
changeNeighbours start (e:ends) xs = changeNeighbours (start++[e]) ends news ++ xs
  where news = [start ++ (c:ends) | c <- edit e] 

editDist s1 s2 = findEditDist s1 [s2] 0

findEditDist s1 s2s 100 = error "not editable"

findEditDist s1 s2s i = do
  if s1 `elem` s2s 
  then return i
  else do
        findEditDist s1 (s2s>>=changeOneNeighbour) (i+1)
  


