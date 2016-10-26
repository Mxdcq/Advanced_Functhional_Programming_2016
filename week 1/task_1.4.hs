removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = removeItem x ys ++ [y]

removeItem0 [] = []
removeItem0 (x:xs) | x == 0 = removeItem0 xs
                 | otherwise = removeItem0 xs ++ [x]