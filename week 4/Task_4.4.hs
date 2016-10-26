--Make a version of Task 3.5 as follows.
--Write a program that gets a key files names as an argument (one for encrypting, one for decrypting), and reads commands from command line. The commands are:
--quit
-- quits the program
--encrypt <s1>
-- encrypts string s1 given as an argument
--decrypt <s1>
-- decrypts string s1 given as an argument
--In encryption, pad the strings with some character (like 'Z') so that the last chunk will be as long as the key matrix row.

import System.Environment 
import System.Directory
import System.IO
import Data.List
import Data.Char

type IntVec = [Int]
type IntMatr = [[Int]]

mult :: IntMatr -> IntVec -> IntVec
mult [] _ = []
mult (a:as) x = (sum $ zipWith (*) a x) : mult as x

key = [[6,24,1],[13, 16, 10],[20, 17, 15]]
invKey = [[8,5,10],[21,8,21],[21,12,8]]

-- vectors are just lists like [0,2,7]

message = "ACTNOW"

numMes s = map (\x -> ord x - ord 'A') s

chrMes s = map (\x -> chr (x + ord 'A')) s

chunkLen = length (head key)

encrypt :: String -> String
encrypt s = doChunks s key
decrypt :: String -> String
decrypt s = doChunks s invKey

doChunks [] _ = []
doChunks s key =  chrMes (map (`mod` 26) (mult key (numMes $ fst (splitAt chunkLen s))))
                  ++ doChunks (snd (splitAt chunkLen s)) key 

main = do
    args <- getArgs
    if (args !! 0) == "encrypt"
    then writeFile "encrypt.txt" (encrypt (args !! 1)) 
    else writeFile "decrypt.txt" (decrypt (args !! 1))