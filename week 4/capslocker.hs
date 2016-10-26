import Control.Monad  
import Data.Char  

{- 
main = forever $ do  
    putStr "Give me some input: "  
    l <- getLine  
    putStrLn $ map toUpper l
-} 
{-
main = do  
    contents <- getContents  
    putStr (shortLinesOnly contents)  
  
shortLinesOnly :: String -> String  
shortLinesOnly input =   
    let allLines = lines input  
        shortLines = filter (\line -> length line < 10) allLines  
        result = unlines shortLines  
    in  result
-}
{-
main = interact shortLinesOnly  
  
shortLinesOnly :: String -> String  
shortLinesOnly input =   
    let allLines = lines input  
        shortLines = filter (\line -> length line < 10) allLines  
        result = unlines shortLines  
    in  result

main = interact $ unlines . filter ((<10) . length) . lines
-}
{-
respondPalindromes contents = unlines (map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") (lines contents))  
    where   isPalindrome xs = xs == reverse xs

respondPalindromes = unlines . map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") . lines  
    where   isPalindrome xs = xs == reverse xs
-}
{-
import System.IO  
  
main = do  
    handle <- openFile "girlfriend.txt" ReadMode  
    contents <- hGetContents handle  
    putStr contents  
    hClose handle
-}
{-
import System.IO     
    
main = do     
    withFile "girlfriend.txt" ReadMode (\handle -> do  
        contents <- hGetContents handle     
        putStr contents)
-}
{-
import System.IO  
  
main = do  
    contents <- readFile "girlfriend.txt"  
    putStr contents
-}
{-
import System.IO     
import Data.Char  
    
main = do     
    contents <- readFile "girlfriend.txt"     
    writeFile "girlfriendcaps.txt" (map toUpper contents)
-}
{-
import System.IO     
    
main = do     
    todoItem <- getLine  
    appendFile "todo.txt" (todoItem ++ "\n")
-}
{-
import System.IO  
import System.Directory  
import Data.List  
  
main = do        
    handle <- openFile "todo.txt" ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let todoTasks = lines contents     
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks     
    putStrLn "These are your TO-DO items:"  
    putStr $ unlines numberedTasks  
    putStrLn "Which one do you want to delete?"     
    numberString <- getLine     
    let number = read numberString     
        newTodoItems = delete (todoTasks !! number) todoTasks     
    hPutStr tempHandle $ unlines newTodoItems  
    hClose handle  
    hClose tempHandle  
    removeFile "todo.txt"  
    renameFile tempName "todo.txt"
-}