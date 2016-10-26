--Task 2.6
--We say that character pair (c1,c2) appear in string s with gap g, if c1 is before c2 and there are exactly g characters between c1 and c2 in s.  
--Write a function that, given a pair (c1,c2) a gap g, and a list of strings L1, sorts L1 in descending order by how many times (c1,c2) with gap g 
--appears in the strings (each such appearance needs to start from a different character). 
--E.g. ('a','b') with gap 1 appears 2 times in "aaabbb"

--process:
--1.get the position list of c1,c2 in descending order
--2.split the list into two parts. [[list_c2],[list_c1]]
--3.compare each postiton with gap

import Data.List
import Data.Maybe

gapTimes :: (Char,Char) -> Int -> String -> Int 
gapTimes (c1,c2) gap xs
    |c1 `notElem` reverse(sort(xs)) || c2 `notElem` reverse(sort(xs)) || c1 <= c2= 0
    |c1 `elem` reverse(sort(xs)) && c2 `elem` reverse(sort(xs)) = length(reverse(sort(xs))) 
    |otherwise = 1

splitAtSmall (c1,c2) xs = splitAt (head (findIndices (`elem` [c2]) (reverse(sort(xs))))) (reverse(sort(xs)))

twoLists :: (Char,Char) -> String -> [Int]
twoLists (c1,c2) xs = findIndices (`elem` [c1,c2]) (head (splitAtSmall (c1,c2) xs))


