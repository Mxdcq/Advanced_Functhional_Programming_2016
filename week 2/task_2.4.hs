--Task 2.4
--Make a function that given a string s removes all characters 'a' from s. Use:
import Data.List

--a) foldl
removeA_foldl xs = foldl (\acc x -> if x == 'a' then acc else acc ++ [x] ) "" xs
--b) foldr
removeA_foldr xs = foldr (\x acc-> if x == 'a' then acc else x:acc ) "" xs
--c) recursion
removeA_recursion "" = ""
removeA_recursion (x:xs) = if x == 'a' then removeA_recursion xs else [x] ++ removeA_recursion xs
--d) list comprehension
removeA_comprehension xs = [x | x <- xs, x /= 'a']
