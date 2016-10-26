--Following book's binary tree example, let's consider binary trees where we store strings. Make functions for:
--- storing strings in the binary tree (storage is like in the book)- finding strings in the tree as follows
--  (a) given strings s1 and s2 find all strings that fall alphabetically between s1 and s2. The result is a list of strings.
--  (b) given string s, find all strings that include all characters of s (if s includes some character n times, then also the 
--   result string has to contain that craracter n times).
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

