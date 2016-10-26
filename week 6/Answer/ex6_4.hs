-- e6.4
import Control.Applicative
import Control.Monad

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq, Ord)

instance Functor Tree where
   fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)
   fmap _ EmptyTree = EmptyTree

instance Applicative Tree where
    pure = leaf
    (<*>) (Node a l r) = fmap a
    (<*>) EmptyTree = const EmptyTree

instance Monad Tree where
   return = leaf
   EmptyTree >>= f = EmptyTree
   (Node x l r) >>= f = f x >>= \y -> Node y (l >>= f) (r >>= f)

leaf :: a -> Tree a
leaf x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = return x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
     | x == a = True
     | x < a  = treeElem x left
     | x > a  = treeElem x right

searchA s1 s2 EmptyTree = []
searchA s1 s2 (Node s leftTree rightTree)
   | (s <= s2) && (s1 <= s) = searchA s1 s2 leftTree ++ [s] ++ searchA s1 s2 rightTree
   | otherwise = searchA s1 s2 leftTree ++ searchA s1 s2 rightTree

searchB s1 EmptyTree = []
searchB s1 (Node s leftTree rightTree)
   | allElem s1 s  = searchB s1 leftTree ++ [s] ++ searchB s1 rightTree
   | otherwise = searchB s1 leftTree ++ searchB s1 rightTree

t = treeInsert "hello" $ treeInsert "hill" $ treeInsert "house" $ treeInsert "apple" EmptyTree

allElem xs s2 = foldr (\ x -> (&&) (x `elem` s2)) True xs