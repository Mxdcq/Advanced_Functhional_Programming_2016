import Data.Char
import Data.List

constantkey = [[6,24,1],[13,16,10],[20,17,15]]
msg= "ACT"

-- letter_to_num msg -> the result will be [[0],[2],[19]]. To use the toInteger function is for solving the type error when I combine this letter_to_num function with matrixMult function below.
letter_to_num [] = []
letter_to_num (x:xs) = if isAlpha x
                       then [toInteger(ord x-65)]:letter_to_num xs 
                       else error "message should only contain [A..Z]"  

-- matrixMult constantkey [[0],[2],[19]] -> the result will be [15,14,7]
matrixMult (x:list1) list2 = ( (flip mod 26) $ sum $ zipWith (*) x ((transpose list2)!!0)):(matrixMult list1 list2)
matrixMult [] y = []

num_to_letter [] = []
num_to_letter (x:xs) =  (chr (fromInteger (x+65))):num_to_letter xs 

-- to test: encode constantkey msg  -> the result will be "POH" ; encode constantkey "CAT" ->the result will be "FIN"                    
encode key message = num_to_letter $ matrixMult key (letter_to_num message) 