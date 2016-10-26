import Data.List

-- Task 4.1

data Point a = Point a a deriving (Show)
type FPoint = Point Float
type Distance = Float

instance Functor Point where
  fmap f (Point x y) = Point (f x) (f y)

closePoints :: (Point Float  -> Point Float -> Distance) -> Point Float -> Distance -> [Point Float]

closePoints f (Point x1 y1) d = 
  [(Point x y) | x<-[(x1 - maxd)..(x1 + maxd)], y<-[(y1 - maxd)..(y1 + maxd)], f (Point x y) (Point x1 y1) <= d]
  where maxd = findMax f (Point x1 y1) d 0

findMax f (Point x1 y1) d maxd 
  | f (Point x1 y1) (Point (x1+maxd) y1) >= d = maxd
  | otherwise = findMax f (Point x1 y1) d (maxd+1) 
   
manh :: Point Float -> Point Float -> Distance
manh (Point x1 y1) (Point x2 y2) = (abs (x1-x2)) + (abs (y1-y2))

-- closeByManh = closePoints manh
closeByManh = closePoints (\(Point x1 y1) (Point x2 y2) -> (abs (x1-x2)) + (abs (y1-y2)))

-- Task 4.2

class Small a where
  isSmall :: a -> Bool

instance Small Int where
  isSmall x 
    | x < 3 && x > -3 = True
    | otherwise = False

instance Small [a] where
  isSmall xs = checkShort xs


checkShort [] = True
checkShort (a:[]) = True
checkShort (a:b:[]) = True
checkShort _ = False
