--Task 2.2
--Write a function that, given two strings s1 and s2, computes a common substring of s1 and s2 as follows. 
--The function finds the earliest common character c (closest to head of either s1 or s2 and appearing in both sequences). 
--The function removes all characters before c in both strings, puts c in the output string, and continues.  
--E.g. s1 = "aabbccdd", s2="bbbbad" -> "bbd".


import Data.List
import Data.Maybe
{-
outputString :: String -> String -> String
outputString "" "" = ""
outputString xs "" = ""
outputString "" ys = ""
outputString (x:xs) (y:ys)
    |x `elem` (y:ys) && y `elem` (x:xs) && ((x `elemIndex` (y:ys)) > (y `elemIndex` (x:xs))) = [y] ++ outputString (drop ((fromJust(y `elemIndex` (x:xs))) + 1) (x:xs)) ys--(drop ((fromJust(y `elemIndex` (y:ys)))) (y:ys))
    |x `elem` (y:ys) && y `elem` (x:xs) && ((x `elemIndex` (y:ys)) < (y `elemIndex` (x:xs))) = [x] ++ outputString xs (drop ((fromJust(x `elemIndex` (y:ys))) + 1) (y:ys))--(drop ((fromJust(x `elemIndex` (x:xs)))) (x:xs))
    |x `elem` (y:ys) && y `elem` (x:xs) && ((x `elemIndex` (y:ys)) == (y `elemIndex` (x:xs))) = [x] ++ outputString xs ys
    |x `notElem` (y:ys) = outputString xs (y:ys)
    |y `notElem` (x:xs) = outputString (x:xs) xs
    |otherwise = outputString xs ys
-}
commonSub [] _ = []
commonSub _ [] = []
commonSub (x:xs) (y:ys)
    |exists1 && exists2 && pos1 < pos2 = x:(commonSub xs (delUntil x (y:ys)))
    |exists1 = x:(commonSub xs (delUntil x (y:ys)))
    |exists2 = y:(commonSub (delUntil y (x:xs)) ys)
    |otherwise commonSub xs ys
    where (exists1, pos1) = pos x (y:ys)
	      (exists2, pos2) = pos y (x:xs)

delUntil - [] = []
delUntil x (y:ys)
    |x == y = ys
    |otherwise = delUntil x ys

pos - [] = (False,0)
pos x (y:ys)
    |x == y = (True, 0)
    |otherwise = ((fst (pos x ys)), (1 + snd (pos x ys)))