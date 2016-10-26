import Control.Concurrent
import Control.Monad
import System.IO
import Data.Char

-- to test, runhaskell e2.hs, then do input like "1 2 3 4".
main = do
  putStrLn "Please input four integers:"  
  line <- getLine  
  let numList = getInts line
  m <- newEmptyMVar
  forkIO $ do putMVar m (numList!!0 + numList!!1); putMVar m (numList!!2 + numList!!3)
  a <- takeMVar m
  print a
  b <- takeMVar m
  print b
  print (a+b)

  
-- getInts "1 2 3" -> the result will be [1,2,3]  
getInts xs =  map strToNumber (words xs) 

-- this function change a string to number  
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