import System.Environment 
import System.Directory
import System.IO
import System.IO.Error
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

main = toTry `catch` handler

toTry :: IO ()
toTry = do args <- getArgs
           if (args !! 0) == "encrypt"
           then writeFile "encrypt.txt" (encrypt (args !! 1)) 
           else writeFile "decrypt.txt" (decrypt (args !! 1))

handler :: IOError -> IO ()  
handler e  
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"    
    | otherwise = ioError e