{-myDrop n xs = if n <= 0 || null xs
              then xs
              else myDrop (n - 1) (tail xs)
-}

{-
-- file: ch03/BookStore.hs
data BookInfo = Book Int String [String]
                deriving (Show)

data MagzineInfo = Magzine Int String [String]
                deriving (Show)
				
myInfo = Book 9780135072455 "Algebra of Programming"
                ["Richard Bird", "Oege de Moor"]

data BookReview = BookReview BookInfo CustomerID String
type CustomerID = Int
type ReviewBody = String
data BetterReview = BetterReview BookInfo CustomerID ReviewBody
type BookRecord = (BookInfo, BookReview)

type CardHolder = String
type CardNumber = String
type Address = [String]
data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)
-}

-- file: ch03/Bool.hs
--data Bool = False | True
{-
-- file: ch03/AlgebraicVector.hs
-- x and y coordinates or lengths.
data Cartesian2D = Cartesian2D Double Double
                   deriving (Eq, Show)

-- Angle and distance (magnitude).
data Polar2D = Polar2D Double Double
               deriving (Eq, Show)
-}

{-
-- file: ch03/Roygbiv.hs
data Roygbiv = Red
             | Orange
             | Yellow
             | Green
             | Blue
             | Indigo
             | Violet
               deriving (Eq, Show)
-}

{-
-- file: ch03/ShapeUnion.hs
type Vector = (Double, Double)

data Shape = Circle Vector Double
           | Poly [Vector]
             deriving (Show)
-}

{-	 
-- file: myNot.hs
myNot True = False
myNot False = True
-}

{-
-- file:: ch03/sumList.hs
--��Ϊ���ӣ�������ֵ sumList [1, 2] ʱ�ᷢ��ʲô�����ȣ� [1, 2] ���ԶԵ�һ����ʽ��ģʽ (x:xs) ����ƥ�䣬�����ģʽƥ��ɹ������� x ��Ϊ 1 �� xs ��Ϊ [2] ��
--������е���һ�������ʽ�ͱ���� 1 + (sumList [2]) �����ǵݹ���� sumList ���� [2] ����ģʽƥ�䡣
--��һ��Ҳ���ڵ�һ����ʽƥ��ɹ������� x ����Ϊ 2 ���� xs ����Ϊ [] �����ʽ��Ϊ 1 + (2 + sumList []) ��
--�ٴεݹ���� sumList ������Ϊ [] ����һ�Σ��ڶ�����ʽ�� [] ģʽƥ��ɹ������� 0 ���������ʽΪ 1 + (2 + (0)) ��������Ϊ 3 ��
sumList (x:xs) = x + sumList xs
sumList []  = 0

haha (x:xs) = (fst x) + (snd x)

--recursion
replicate' :: (Num i, Ord i) => i -> a -> [a]   
replicate' n x   
    | n <= 0    = []   
    | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]   
take' n _   
    | n <= 0   = []   
take' _ []     = []   
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]   
reverse' [] = []   
reverse' (x:xs) = reverse' xs ++ [x]

--quicksort
quicksort :: (Ord a) => [a] -> [a]   
quicksort [] = []   
quicksort (x:xs) =   
  let smallerSorted = quicksort [a | a <- xs, a <= x]  
       biggerSorted = quicksort [a | a <- xs, a > x]   
  in smallerSorted ++ [x] ++ biggerSorted
  
--quicksort with filter
quicksort :: (Ord a) => [a] -> [a]     
quicksort [] = []     
quicksort (x:xs) =      
    let smallerSorted = quicksort (filter (<x) xs) 
        biggerSorted = quicksort (filter (>x) xs)    
    in  smallerSorted ++ [x] ++ biggerSorted

repeat' :: a -> [a]   
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a,b)]   
zip' _ [] = []   
zip' [] _ = []   
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool   
elem' a [] = False   
elem' a (x:xs)   
    | a == x    = True   
    | otherwise = a `elem'` xs
-}

{-
--map
map :: (a -> b) -> [a] -> [b]   
map _ [] = []   
map f (x:xs) = f x : map f xs

map (+3) [1,5,3,1,6]
--[4,8,6,4,9]
map (++ "!") ["BIFF"��"BANG"��"POW"]  
--["BIFF!","BANG!","POW!"]
map (replicate 3) [3..6]
--[[3,3,3],[4,4,4],[5,5,5],[6,6,6]]
map (map (^2)) [[1,2],[3,4,5,6],[7,8]] 
--[[1,4],[9,16,25,36],[49,64]]
map fst [(1,2),(3,5),(6,3),(2,6),(2,5)]
--[1,3,6,2,2]
-}

{-
--filter
filter :: (a -> Bool) -> [a] -> [a]   
filter _ [] = []   
filter p (x:xs)    
    | p x       = x : filter p xs   
    | otherwise = filter p xs
	
filter (>3) [1,5,3,2,1,6,4,3,2,1] 
--[5,6,4]
filter (==3) [1,2,3,4,5] 
--[3]
filter even [1..10]
--[2,4,6,8,10]
let notNull x = not (null x) in filter notNull [[1,2,3],[],[3,4,5],[2,2],[],[],[]]
--"uagameasadifeent"
filter (`elem` ['a'..'z']) "u LaUgH aT mE BeCaUsE I aM diFfeRent"
--"GAYBALLS"
-}

change a b = let a = b






















