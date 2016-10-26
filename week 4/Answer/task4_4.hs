module Main where

import Data.List 
import Data.Char
import System.Environment

type IntVec = [Int]
type IntMatr = [[Int]]

main::IO()
main = do
    (file1:files) <- getArgs
    contents <- readFile file1
    contents2 <- readFile (head files)
    let key = getKeys (lines contents)
        invKey = getKeys (lines contents2)
    waitAction key invKey

getKeys :: [String] -> IntMatr
getKeys [] = []
getKeys (x:xs) = (strToInt (words x)) : (getKeys xs)

strToInt :: [String] -> IntVec
strToInt [] = []
strToInt (x:xs) = (read x :: Int) : (strToInt xs) 
    
waitAction :: IntMatr -> IntMatr -> IO ()
waitAction eKeys dKeys = do
    putStrLn "Give command: "
    commands <- getLine
    let (cmd:args) = words commands
        result = (readAction (cmd:args) eKeys dKeys)
    putStrLn result
    if result == "Quitting"
        then return ()
        else waitAction eKeys dKeys
    
    
readAction :: [String] -> IntMatr -> IntMatr -> String
readAction (cmd:args) eKeys dKeys
    | cmd == "quit" = "Quitting"
    | cmd == "encrypt" = encrypt (head args) eKeys
    | cmd == "decrypt" = decrypt (head args) dKeys
    | otherwise = "False commands. Used ones are 'quit', 'encrypt' and 'decrypt'"

mult :: IntMatr -> IntVec -> IntVec
mult [] _ = []
mult (a:as) x = (sum $ zipWith (*) a x) : mult as x


numMes s = map (\x -> ord x - ord 'A') s

chrMes s = map (\x -> chr (x + ord 'A')) s

chunkLen key = length (head key)

encrypt :: String -> [[Int]] -> String
encrypt s key = doChunks s key

decrypt s invKey = doChunks s invKey

doChunks [] _ = []
doChunks s key =  chrMes (map (`mod` 26) (mult key (numMes $ fst (splitAt (chunkLen key) s))))
                  ++ doChunks (snd (splitAt (chunkLen key) s)) key
