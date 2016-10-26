subString :: [Char] -> [Char] -> [Char]
dropHead :: [Char] -> Char -> [Char]
subString xs [] = []
subString [] ys = []
subString all@(x:xs) (y:ys) = 
	if (elem y all) 
		then [y] ++ subString (dropHead all y) ys
        else subString all ys

dropHead (a:list) b = 
	if (a == b) 
		then list
        else dropHead list b