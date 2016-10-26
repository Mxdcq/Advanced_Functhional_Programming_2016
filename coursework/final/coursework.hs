import System.IO
import Data.Char

main = do
--    handle <- openFile "untagged.txt" ReadMode
--    contents <- hGetContents handle
--    putStr contents
--    hClose handle
    contents <-readFile "untagged.txt"
    writeFile "result.txt" contents