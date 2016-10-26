import Test.QuickCheck

reverseNoZeros [] = []
reverseNoZeros (0:xs) = reverseNoZeros xs
reverseNoZeros (x:xs) = reverseNoZeros xs ++ [x]

prop_idempotent xs = reverseNoZeros (reverseNoZeros (reverseNoZeros xs)) == reverseNoZeros xs

main = do 
   quickCheck (prop_idempotent :: [Integer] -> Bool)