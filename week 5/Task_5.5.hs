import Data.Monoid

newtype DiffList a = DiffList { getDiffList :: [a] -> [a]  }

toDiffList :: [a] -> DiffList a  
toDiffList xs = DiffList (xs++)  
      
fromDiffList :: DiffList a -> [a]  
fromDiffList (DiffList f) = f []  

instance Monoid (DiffList a) where  
    mempty = DiffList (\xs -> [] ++ xs)  
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs)) 

instance (Eq a) => Eq (DiffList a) where
    (DiffList x) == (DiffList y) = fromDiffList (DiffList x) == fromDiffList (DiffList y)

instance (Show a) => Show (DiffList a) where
    show (DiffList x) = show (fromDiffList (DiffList x))