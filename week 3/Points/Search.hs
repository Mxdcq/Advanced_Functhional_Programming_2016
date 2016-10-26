module Points.Search
( closePoints ) where

import qualified Points.Basic as Basic

-- testing: a = (Basic.Point 1.9 1,8)

-- Task 3.1

closePoints :: (Basic.Point -> Basic.Point -> Basic.Distance) -> Basic.Point -> Basic.Distance -> [Basic.Point]

closePoints f (Basic.Point x1 y1) d = 
  [(Basic.Point x y) | x<-[(x1 - maxd)..(x1 + maxd)], y<-[(y1 - maxd)..(y1 + maxd)], f (Basic.Point x y) (Basic.Point x1 y1) <= d]
  where maxd = findMax f (Basic.Point x1 y1) d 0

findMax f (Basic.Point x1 y1) d maxd 
 | f (Basic.Point x1 y1) (Basic.Point (x1+maxd) y1) >= d = maxd
 | otherwise = findMax f (Basic.Point x1 y1) d (maxd+1) 
   
