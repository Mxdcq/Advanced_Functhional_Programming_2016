--main = putStrLn "hello, world" 

{-
main = do  
    putStrLn "Hello, what's your name?"  
    name <- getLine  
    putStrLn ("Hey " ++ name ++ ", you rock!")
-}

{-
main = do  
    putStrLn "Hello, what's your name?"  
    name <- getLine  
    putStrLn $ "Read this carefully, because this is your future: " ++ tellFortune name
-}

{-
import Data.Char    
main = do  
    putStrLn "What's your first name?"  
    firstName <- getLine  
    putStrLn "What's your last name?"  
    lastName <- getLine  
    let bigFirstName = map toUpper firstName  
        bigLastName = map toUpper lastName  
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?" 
-}

{-
main = do   
    line <- getLine  
    if null line  
        then return ()  
        else do  
            putStrLn $ reverseWords line  
            main  
  
reverseWords :: String -> String  
reverseWords = unwords . map reverse . words 
-}

{-
main = do  
    return ()  
    return "HAHAHA"  
    line <- getLine  
    return "BLAH BLAH BLAH"  
    return 4  
    putStrLn line
-}

{-
main = do  
    a <- return "hell"  
    b <- return "yeah!"  
    putStrLn $ a ++ " " ++ b 

main = do  
    let a = "hell"  
        b = "yeah"  
    putStrLn $ a ++ " " ++ b 
-}

{-
main = do   putStr "Hey, "  
            putStr "I'm "  
            putStrLn "Andy!"

main = do   putChar 't'  
            putChar 'e'  
            putChar 'h'

putStr :: String -> IO ()  
putStr [] = return ()  
putStr (x:xs) = do  
    putChar x  
    putStr xs

main = do   print True  
            print 2  
            print "haha"  
            print 3.2  
            print [3,4,3]

main = do     
    c <- getChar  
    if c /= ' '  
        then do  
            putChar c  
            main  
        else return ()

import Control.Monad   
  
main = do  
    c <- getChar  
    when (c /= ' ') $ do  
        putChar c  
        main

main = do  
    rs <- sequence [getLine, getLine, getLine]  
    print rs

main = do  
    a <- getLine  
    b <- getLine  
    c <- getLine  
    print [a,b,c]

import Control.Monad  
import Data.Char  
  
main = forever $ do  
    putStr "Give me some input: "  
    l <- getLine  
    putStrLn $ map toUpper l

import Control.Monad  
  
main = do   
    colors <- forM [1,2,3,4] (\a -> do  
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"  
        color <- getLine  
        return color)  
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "  
    mapM putStrLn colors


-}

{-
import System.Environment   
import Data.List  
  
main = do  
   args <- getArgs  
   progName <- getProgName  
   putStrLn "The arguments are:"  
   mapM putStrLn args  
   putStrLn "The program name is:"  
   putStrLn progName 
-}

{-
import System.Environment   
import System.Directory  
import System.IO  
import Data.List  
  
dispatch :: [(String, [String] -> IO ())]  
dispatch =  [ ("add", add)  
            , ("view", view)  
            , ("remove", remove)  
            ]

add :: [String] -> IO ()  
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()  
view [fileName] = do  
    contents <- readFile fileName  
    let todoTasks = lines contents  
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks  
    putStr $ unlines numberedTasks

remove :: [String] -> IO ()  
remove [fileName, numberString] = do  
    handle <- openFile fileName ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let number = read numberString  
        todoTasks = lines contents  
        newTodoItems = delete (todoTasks !! number) todoTasks  
    hPutStr tempHandle $ unlines newTodoItems  
    hClose handle  
    hClose tempHandle  
    removeFile fileName  
    renameFile tempName fileName

main = do  
    (command:args) <- getArgs  
    let (Just action) = lookup command dispatch  
    action args
-}

