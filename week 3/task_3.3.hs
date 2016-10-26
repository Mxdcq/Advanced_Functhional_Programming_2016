{-Following book's binary tree example, let's consider binary trees where we store strings. Make functions for:
- storing strings in the binary tree (storage is like in the book)- finding strings in the tree as follows
  (a) given strings s1 and s2 find all strings that fall alphabetically between s1 and s2. The result is a list of strings.
  (b) given string s, find all strings that include all characters of s (if s includes some character n times, then also the result string has to contain that craracter n times).
  -}

storeBook :: [(String, String)]
storeBook =      
    [("betty","555-2938")     
    ,("bonnie","452-2928")     
    ,("patsy","493-2928")     
    ,("lucille","205-2928")     
    ,("wendy","939-8282")     
    ,("penny","853-2492")     
    ] 

findBook :: String -> String -> [(String, String)] -> [String]
findBook s1 s2 [] = []
findBook s1 s2 (storeBook:xs)
    |s1 <= fst storeBook && fst storeBook <= s2 = (fst storeBook) : findBook s1 s2 xs
    |s1 <= snd storeBook && fst storeBook <= s2 = (snd storeBook) : findBook s1 s2 xs
    |otherwise = findBook s1 s2 xs

findBook' :: String -> [(String, String)] -> [String]
findBook' "" [] = []
findBook' s1 [] = []
findBook' xs (storeBook:ys) 
    |inString xs (fst storeBook) = (fst storeBook) : findBook' xs ys
    |inString xs (snd storeBook) = (snd storeBook) : findBook' xs ys
    |otherwise = findBook' xs ys

inString "" "" = False
inString "" ys = True
inString (x:xs) y
 | x `elem` y = inString xs y
 | otherwise = False