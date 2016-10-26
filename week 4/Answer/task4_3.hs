-- Make a version of Task 3.3 as follows. Write a program that gets a 
-- filename as an argument, reads strings from the file and builds a tree, 
-- and then waits for input. The search commands and parameters are read as 
-- input and the search result is printed out as output. You can specify the 
-- input and output formats yourself.
import System.Environment   
import Data.List  

main = do
    args <- getArgs
    contents <- readFile (args !! 0)
    
    putStrLn ("Reading file " ++ (args !! 0) ++ " and building a tree...")
    putStrLn "Select the search method [searchA/searchB]"
    method <- getLine 
            
    if (method == "searchA")
    then do
        putStrLn "Find all strings that fall alphabetically between s1 and s2:"
        putStrLn "String 1 (press Enter when ready):"
        s1 <- getLine
        putStrLn "String 2 (press Enter when ready):"
        s2 <- getLine
        putStrLn ("Results: \n" ++(unlines (searchA s1 s2 (addWordsIntoTree (lines contents)))))
    else (putStrLn (""))

    if (method == "searchB")
    then do 
        putStrLn "Find all strings that include all characters of given input:"
        word <- getLine
        putStrLn ("Results: \n" ++(unlines (searchB word (addWordsIntoTree (lines contents)))))
    else (putStrLn "Unknown command!")

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


addWordsIntoTree :: [String] -> Tree String
addWordsIntoTree [] = EmptyTree
addWordsIntoTree (x:xs) = if (length xs > 0)
                          then (treeInsert x (addWordsIntoTree xs))
                          else treeInsert x EmptyTree


allElem [] s2 = True
allElem (x:xs) s2 =
   if (x `elem` s2) then
      allElem xs s2
   else False
   
