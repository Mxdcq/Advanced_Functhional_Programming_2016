--Rewrite the solution for 3.1 in such a way that 
--the basic data type definitions and distance function are in one module 
--and the function to find points in another.
module FindPoints
( closePoints
, closeByManh
) where

import PointDistance

closePoints :: (Point -> Point -> Distance) -> Point -> Distance -> [Point]

closePoints f (Point x1 y1) d = 
  [(Point x y) | x<-[(x1 - maxd)..(x1 + maxd)], y<-[(y1 - maxd)..(y1 + maxd)], f (Point x y) (Point x1 y1) <= d]
  where maxd = findMax f (Point x1 y1) d 0

findMax f (Point x1 y1) d maxd 
  | f (Point x1 y1) (Point (x1 + maxd) y1) >= d = maxd
  | otherwise = findMax f (Point x1 y1) d (maxd+1)

closeByManh = closePoints (\(Point x1 y1) (Point x2 y2) -> (abs (x1-x2)) + (abs (y1-y2)))