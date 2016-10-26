import Data.Monoid
import qualified Data.Foldable as F

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

instance Functor Tree where
    fmap g EmptyTree             = EmptyTree
    fmap g ( Node a tl tr )  = Node a ( fmap g tl ) ( fmap g tr )

instance Monoid (Tree a) where
    mempty = EmptyTree
    mappend = Node

instance F.Foldable Tree where  
    foldMap f EmptyTree = mempty  
    foldMap f (Node x l r) = F.foldMap f l `mappend`  
                             f x           `mappend`  
                             F.foldMap f r