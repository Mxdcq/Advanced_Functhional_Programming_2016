--Task 2.5
--We say that character pair (c1,c2) appear in string s with gap g, if c1 is before c2 and there are exactly g characters between c1 and c2 in s.  
--Write a function that, given a gap g and strings s1 and s2, finds all character pairs that appear with gap g in both s1 and s2. 
--E.g. if gap = 2 and s1 ="afcfa" and s2="aaaaafff", then there is just one such character pair: ('a','f')

findPairs :: [Char] -> [Char] -> Int -> [(Char,Char)]
dropHead :: [Char] -> Char -> [Char]
findPairs [] [] g = []
findPairs (x:xs) [] g = []
findPairs [] (y:ys) g = []
findPairs (x:xs) (y:ys) g =
	if (elem x (y:ys)) 
		then 
			if ((head (drop (g + 1) (dropHead (y:ys) x))) == (head (drop (g + 1) (x:xs))))
				then (x,(head (drop (g + 1) (x:xs)))): findPairs xs (y:ys) g
				else findPairs xs (y:ys) g
        else findPairs xs (y:ys) g

dropHead (a:list) b = 
	if (a == b) 
		then [b] ++ list
        else dropHead list b		