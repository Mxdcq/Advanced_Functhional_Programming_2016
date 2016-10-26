import Data.Monoid

data Point a = Point a a deriving (Show)

--instance Functor Point where
--  fmap f (Point x y) = Point (f x) (f y)

instance Num a => Monoid (Point a) where
  mempty = Point 0 0
  mappend (Point x1 y1) (Point x2 y2) = Point (x1+x2) (y1+y2)