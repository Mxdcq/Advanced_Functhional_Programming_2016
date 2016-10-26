--positivePoint x y = if sqrt(x**2+y**2)<=4.0
--                    then "The distance from " ++ "(" ++ show(x) ++ "," ++ show(y) ++ ")" ++ " to (0,0) is at most 4"
--                    else "The distance from " ++ "(" ++ show(x) ++ "," ++ show(y) ++ ")" ++ " to (0,0) is more than 4"

--pointList [] = error "Done"
--pointList (x:xs) | ((fst x)**2 + (snd x)**2) <= 16 = "The distance from " ++ show(x) ++ " to (0,0) is at most 4"
--                 | ((fst x)**2 + (snd x)**2) > 16 = pointList xs
--				 | otherwise = pointList xs

pointList xs = [(x,y) | (x,y) <- xs, x**2 + y**2 <= 16]
distanceList xs = [sqrt (x**2 + y**2) | (x,y) <- xs, x**2 + y**2 <= 16]
				 

{-
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = removeItem x ys ++ [y]

removeItem0 [] = []
removeItem0 (x:xs) | x == 0 = removeItem0 xs
                 | otherwise = removeItem0 xs ++ [x]
-}