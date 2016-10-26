import Data.List

data Point = Point Float Float deriving (Show)
type Distance = Float

closePoints :: (Point -> Point -> Distance) -> Point -> Distance -> [Point]

closePoints f (Point x1 y1) d = 
  [(Point x y) | x<-[(x1 - maxd)..(x1 + maxd)], y<-[(y1 - maxd)..(y1 + maxd)], f (Point x y) (Point x1 y1) <= d]
  where maxd = findMax f (Point x1 y1) d 0

findMax f (Point x1 y1) d maxd 
  | f (Point x1 y1) (Point (x1 + maxd) y1) >= d = maxd
  | otherwise = findMax f (Point x1 y1) d (maxd+1) 
   
f :: Point -> Point -> Distance
f (Point x1 y1) (Point x2 y2) = (abs (x1 - x2) + abs (y1 - y2))

-- closeByManh = closePoints manh
closeByManh = closePoints (\(Point x1 y1) (Point x2 y2) -> (abs (x1-x2)) + (abs (y1-y2)))