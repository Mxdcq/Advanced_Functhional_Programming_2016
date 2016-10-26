--Make type Point a member of the Functor typeclass.
--Hint: Functor requires a parameterized type (like [] has a type parameter). So, you may do something like:
--data Point a = ...
--to give Point a type parameter.
data Point a = Point a a  deriving (Show)

instance Functor Point where
  fmap f (Point x y) = Point (f x) (f y)