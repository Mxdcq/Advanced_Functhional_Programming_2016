--The book contains an example of a simple cipher (skipped in the lectures, but easy to follow).
--Implement the Hill cipher: https://en.wikipedia.org/wiki/Hill_cipher
--You may use a constant key, like the one in the Wikipedia article.

import Data.Char
import Data.List

constantkey = [[6,24,1],[13,16,10],[20,17,15]]
msg= "ACT"

letter_to_num [] = []
letter_to_num (x:xs) = if isAlpha (toUpper(x))
                       then [toInteger(ord (toUpper(x))-65)]:letter_to_num xs 
                       else error "message should only contain [A..Z] or [a..z]"  

matrixMult [] y = []
matrixMult (x:list1) list2 = ( (flip mod 26) $ sum $ zipWith (*) x ((transpose list2)!!0)):(matrixMult list1 list2)

num_to_letter [] = []
num_to_letter (x:xs) =  (chr (fromInteger (x+65))):num_to_letter xs 
   
encode key message = num_to_letter $ matrixMult key (letter_to_num message) 
