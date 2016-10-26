--The book contains an example of a simple cipher (skipped in the lectures, but easy to follow).
--Implement the Hill cipher: https://en.wikipedia.org/wiki/Hill_cipher
--You may use a constant key, like the one in the Wikipedia article.

module Main (main) where

import qualified Data.List as L

alphabet :: [(Char, Int)]
alphabet = zip (['A'..'Z'] ++ ['a'..'z']) ([0..25] ++ [0..25])

substituteNumber :: [Int] -> String
substituteNumber = map charNumber 

substituteAlpha :: String -> [Int]
substituteAlpha = map enumLetter

enumLetter :: Char -> Int
enumLetter a = snd . head $ filter (\(b, _) -> b == a) alphabet

charNumber :: Int -> Char
charNumber n = fst . head $ filter (\(_, b) -> b == n) alphabet

splitBy :: Int -> String -> [String]
splitBy c = splitBy' c []
            where 
            splitBy' _ acc []   = acc
            splitBy' n acc list = let (f, rest) = L.splitAt n list
                                       in
                                       acc ++ splitBy' n [f] rest

--VERY HACKISH SOLUTION but this will work for any correctly
--formed square "matrix" list
ncols :: [[Int]] -> Int
ncols = maximum . map length

-- This must be a square matrix
-- i.e. the number of sublists must be equal
-- to the number of elements in every sublist
-- all sublists must have an equal number of elements
hillCipherMatrix :: [[Int]]
hillCipherMatrix = [
                     [ 5, 2, 12]
                   , [11, 9,  4]
                   , [ 8, 1, 22]
                   ]

matrixMult :: [[Int]] -> [Int] -> [Int]
matrixMult xs ks = map (rowMult ks) xs 
                   where
                   rowMult cs rs = sum . map (uncurry (*)) $ zip cs rs

hillCipher :: String -> String
hillCipher xs = 
    concatMap encipher xs'
    where
    xs' = splitBy cipherRows xs 
    cipherRows = ncols hillCipherMatrix 
    encipher cs = 
        substituteNumber . map (`mod` 26) $ hillCipherMatrix `matrixMult` substituteAlpha cs

main :: IO ()
main = putStrLn $ hillCipher "KNIGHT"