import Control.Applicative

data Point a = Point a a  deriving (Show)

instance Functor Point where
    fmap f (Point x y) = Point (f x) (f y)

instance Applicative Point where  
    pure x = Point x x
    Point f1 f2 <*> Point x y = Point (f1 x) (f2 y)

task_1_2 :: Point -> Int -> [Point]
task_1_2 (Point x1 y1) d = [(Point x y) | (Point x y) <- (+) <$> Point f1 f2 <*> Point x y, ((x-x1)^2+(y-y1)^2)<=d^2]
    where f1 = [(-d),(-d+1)..d]
	      f2 = [(-d),(-d+1)..d]

--task2 :: (Int,Int) -> Int -> [(Int,Int)]
--task2 (x1,y1) d = [(x,y) | x<-[(x1-d)..(x1+d)], y<-[(y1-d)..(y1+d)], ((x-x1)^2+(y-y1)^2)<=d^2]




