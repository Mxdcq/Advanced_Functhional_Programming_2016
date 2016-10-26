import Data.Monoid
import Data.List (sort)
import Control.Applicative

-- ex 5.1

data Point a = Point a a deriving (Show)

instance Num x => Monoid (Point x) where
    mempty = Point 0 0
    (Point a b) `mappend` (Point c d) = Point (a+c) (b+d)


-- ex 5.2



instance Functor Point where
    fmap f (Point x y) = Point (f x) (f y)

instance Applicative Point where
  pure x = Point x x
  Point f g <*> Point x y = Point (f x) (g y)

points :: Point Int -> Float -> [Point Int]
points (Point x y) d = filter (\(Point x1 y1) -> (fromIntegral((x1-x)^2) + fromIntegral((y1-y)^2)) <= d^2) $ Point <$> [x-(ceiling d)..x+(ceiling d)] <*> [y-(ceiling d)..y+(ceiling d)]

-- ex 5.3

type Stack a = [a]

start :: Maybe (Stack a)
start = return []

val :: (Num a) => a -> Stack a -> Maybe (Stack a)
val x = \s -> return $ x : s

twoOp :: (a -> a -> Maybe a) -> Stack a -> Maybe (Stack a)
twoOp f []          = Nothing
twoOp f (x1:[])     = Nothing
twoOp f (x2:x1:xs)  = case (f x1 x2) of
  Just x  -> Just (x : xs)
  Nothing -> Nothing

div' :: (Integral a) => Stack a -> Maybe (Stack a)
div' = twoOp $ \x y -> case y of
  0         -> Nothing
  otherwise -> Just $ x `div` y

mul :: (Num a) => Stack a -> Maybe (Stack a)
mul = twoOp $ \x y -> Just $ x * y

minus :: (Num a) => Stack a -> Maybe (Stack a)
minus = twoOp $ \x y -> Just $ x - y

plus :: (Num a) => Stack a -> Maybe (Stack a)
plus = twoOp $ \x y -> Just $ x + y

test = start >>= val 1 >>= val 3 >>= val 2 >>= minus >>= plus >>= val 5 >>= mul >>= val 2 >>= div'

-- ex5.4

instance Ord a => Monoid (Tree a) where
    mempty = Empty
    mappend Empty tree = tree
    mappend tree Empty = tree
    mappend tree1 tree2 =
        let allvals = merge (vals tree1) (vals tree2)
        in foldl (\tree e -> insert e tree) Empty allvals

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x Empty Empty
  
insert :: (Ord a) => a -> Tree a -> Tree a
insert x Empty = singleton x
insert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (insert x left) right
    | x > a  = Node a left (insert x right)

vals :: Tree a -> [a]
vals Empty = []
vals (Node a left right) = (vals left) ++ [a] ++ (vals right)

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | x > y  = y : merge (x:xs) ys

a = insert 10 $ insert 9 $ insert 5 $ insert 6 $ mempty
-- Node 6 (Node 5 Empty Empty) (Node 9 Empty (Node 10 Empty Empty))

b = insert 3 $ insert 7 $ insert 6 $ insert 4 $ mempty
-- Node 4 (Node 3 Empty Empty) (Node 6 Empty (Node 7 Empty Empty))

c = insert 2 $ insert 8 $ insert 1 $ insert 0 $ mempty
-- Node 0 Empty (Node 1 Empty (Node 8 (Node 2 Empty Empty) Empty))

-- ex5.5


newtype Mylist a = Mylist { getMylist :: [a] }  
    deriving (Eq, Ord, Read, Show)  

instance (Ord a) => Monoid (Mylist a) where  
    mempty = Mylist []
    mappend (Mylist []) l = l
    mappend l (Mylist []) = l
    mappend (Mylist x) (Mylist y) = (Mylist (sort $ x++y))
