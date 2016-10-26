{-isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False
 
isDouble s = case reads s :: [(Double, String)] of
  [(_, "")] -> True
  _         -> False
 
--isNumeric :: String -> Bool
isNumeric s = if isInteger s || isDouble s then s else "error"
-}


--I did a elementary version and one of my senior improved it.
import Data.List
import System.IO

isNumeric [] = 0

isNumeric [x]  
    |x == '0' = 1
    |x == '1' = 1
    |x == '2' = 1
    |x == '3' = 1
    |x == '4' = 1
    |x == '5' = 1
    |x == '6' = 1
    |x == '7' = 1
    |x == '8' = 1
    |x == '9' = 1
    |otherwise = 0

isNumeric (x:xs)
    |x == '0' = isNumeric xs
    |x == '1' = isNumeric xs
    |x == '2' = isNumeric xs
    |x == '3' = isNumeric xs
    |x == '4' = isNumeric xs
    |x == '5' = isNumeric xs
    |x == '6' = isNumeric xs
    |x == '7' = isNumeric xs
    |x == '8' = isNumeric xs
    |x == '9' = isNumeric xs
    |otherwise = 0

interpret str= if isNumeric str == 0
               then error "not a number"
               else read str::Int