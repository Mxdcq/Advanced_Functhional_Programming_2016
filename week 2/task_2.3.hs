--Task 2.3
--Write a function that take as parameters:
--- a function f that, given two Int coordinate points, calculates their distance,
--- an Int coordinate point p, and
--- an distance d (also Int),
--calculates a list of such points that using the distance function f, the points' distance from the parameter point p is at most d.
--Your function's type should be:
--((Int,Int) -> (Int,Int) -> Int) -> (Int,Int) -> Int -> [(Int,Int)]    
--After that, use partial evaluation to create another function, where f has been fixed to the Manhattan distance function, by using partial evaluation. 
--Do that by fixing f with a lambda funciton. Manahttan distance is the x-distance + y-distance between the points. 
--E.g. Manhattan distance between (2,1) and (3,3) is 1+2 = 3.


--Maybe I misunderstand the question
f :: (Int,Int) -> (Int,Int) -> Int
f point_one point_two = abs (fst point_one - fst point_two) + abs (snd point_one - snd point_two)

nearPoints :: ((Int,Int) -> (Int,Int) -> Int) -> (Int,Int) -> Int -> [(Int,Int)]
nearPoints f point distance = [(x,y) | x <- [(fst point - distance)..(fst point + distance)], y <- [(snd point - distance)..(snd point + distance)], f (x,y) point <= distance]

nearPoints' point distance = filter (\(x,y) -> f (x,y) point <= distance)[(x,y) | x <- [(fst point - distance)..(fst point + distance)], y <- [(snd point - distance)..(snd point + distance)]]