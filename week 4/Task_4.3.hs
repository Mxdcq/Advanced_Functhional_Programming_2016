import System.IO
import Data.Char(toUpper)
import System.Directory  
import Data.List 
import System.Environment
  
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

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

main :: IO ()
main = do 
    inh	<- openFile "input.txt" ReadMode
    outh <- openFile "output.txt" WriteMode
    mainloop inh outh
    hClose inh
    hClose outh

mainloop :: Handle -> Handle -> IO ()
mainloop inh outh = 
    do ineof <- hIsEOF inhs
	    if ineof
        then return ()
        else do inStr <- hGetChar inh
		        hPutStr outh (treeInsert inStr outh)
                mainloop inh outh
      