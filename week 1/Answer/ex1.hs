
-- task1.1
task1 ::  [(Int,Int)]
-- I wrote the 4::Int to give the Int type explicitly, otherwise
-- it seemd to become Integer
task1 = [(x,y) | x<-[0..4], y<-[0..4], x^2+y^2<=(4::Int)^2]

-- task1.2
task2 :: (Int,Int) -> Int -> [(Int,Int)]
task2 (x1,y1) d = 
  [(x,y) | 
   x<-[(x1-d)..(x1+d)], y<-[(y1-d)..(y1+d)], ((x-x1)^2+(y-y1)^2)<=d^2]

-- task1.3
hello "hello" "world" = "Hello!"
hello "hello" x = "Hello, but I am not " ++ x
hello _ _ = "Sorry, I do not understand"

-- task 1.4
reverseNoZeros [] = []
reverseNoZeros (0:xs) = reverseNoZeros xs
reverseNoZeros (x:xs) = reverseNoZeros xs ++ [x]

-- task1.5, not very sophisticated
strToNumber "" = error "Empty string"
strToNumber s = strToNo s 0 (length s)
strToNo [] num l = num 
strToNo (x:xs) num l = (strToNo xs num (l-1)) + ((numValue x)* 10^(l-1))

numValue '0' = 0
numValue '1' = 1
numValue '2' = 2
numValue '3' = 3
numValue '4' = 4
numValue '5' = 5
numValue '6' = 6
numValue '7' = 7
numValue '8' = 8
numValue '9' = 9
numValue x = error ([x] ++ " does not seem like a part of anumber")

