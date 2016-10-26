-- Task 2.1

removeChar s1 c = removeChar' s1 c []

removeChar' []     _ res = res
removeChar' (x:xs) c res
    | x == c    = res ++ xs
    | otherwise = removeChar' xs c (res ++ [x])

removeChars s1 [] = s1
removeChars s1 (y:ys) = removeChars (removeChar s1 y) ys

-- *Main> removeChars "aabbccdd" "bbbbad"
-- "accd"

-- Task 2.2

-- This solution includes unnecessary optimization:
commonSub [] _ = []
commonSub _ [] = []
commonSub (x:xs) (y:ys)
 | exists1 && exists2 && pos1 < pos2 = x : (commonSub xs (delUntil x (y:ys)))
 | exists1 = x : (commonSub xs (delUntil x (y:ys)))
 | exists2 = y : (commonSub (delUntil y (x:xs)) ys )
 | otherwise = commonSub xs ys
 where (exists1, pos1) = pos x (y:ys)
       (exists2, pos2) = pos y (x:xs)

-- Simpler:
commonSub' [] _ = []
commonSub' _ [] = []
commonSub' (x:xs) (y:ys)
  | x `elem` (y:ys) = x : (commonSub' xs (delUntil x (y:ys)))
  | y `elem` (x:xs) = y : (commonSub' (delUntil y (x:xs)) ys )
  | otherwise = commonSub' xs ys


delUntil _ [] =  []
delUntil x (y:ys)
  | x==y = ys
  | otherwise = delUntil x ys

-- Only needed in the more complicated version:
pos _ [] = (False, 0)
pos x (y:ys)
 | x==y = (True, 0)
 | otherwise = ((fst (pos x ys)), (1 + snd (pos x ys)))



-- Task 2.4

task2_4a s = foldl (\acc x -> if x=='a' then acc else acc ++ [x]) [] s 
task2_4b s = foldr (\x acc -> if x=='a' then acc else x : acc) [] s
task2_4c [] = []
task2_4c (x:xs)
 | x=='a' = task2_4c xs
 | otherwise = x : task2_4c xs
task2_4d s = [x | x<-s, x/= 'a'] 
