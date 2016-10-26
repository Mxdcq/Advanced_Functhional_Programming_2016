import Data.List

deleteCharacters :: String -> String -> String 
deleteCharacters "" "" = ""
deleteCharacters xs "" = xs
deleteCharacters "" _ = ""
deleteCharacters (x:xs) (y:ys)
    |y `notElem` (x:xs)= deleteCharacters (x:xs) ys
    |otherwise = deleteCharacters (delete y (x:xs)) ys