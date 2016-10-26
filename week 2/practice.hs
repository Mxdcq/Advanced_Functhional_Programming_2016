import Data.List 

commonStr :: String -> String -> String
commonStr _ [] = []
commonStr [] _ = []
commonStr (x:xs) (y:ys) 

                  | posx >= posy  =  y: commonStr (drop posy (x:xs)) (drop 1 (y:ys)) 
		 | otherwise     =  x: commonStr (drop 1 (x:xs)) (drop posx (y:ys)) 		 
		 where posx = position x (y:ys) 
		       posy = position y (x:xs)


position :: Char -> String -> Int
position x ys =  length (takeWhile (/=x) ys) +1