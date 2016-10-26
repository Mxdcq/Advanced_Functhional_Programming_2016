import Data.Monad

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show, Read)

instance Monad Tree where
    return x = Node x EmptyTree EmptyTree
    EmptyTree >>= f  = EmptyTree
    Node a l r >>= f = f x >>= \y -> Node y (l >>= f) (r >>= f)
    fail _ = EmptyTree