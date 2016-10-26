import Data.Monoid
import Control.Monad.Writer
import Control.Monad.Instances
import Control.Monad.Error
import Control.Monad
import Control.Monad.Writer.Lazy
import Data.Ratio
--import Control.Monad.State

-- **** Writer
-- Writer: logging context
isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")

applyLog' :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog' (x, log) f  = let (y, newLog) = f x in (y, log ++ newLog)

main1 = ("Tobin", "Got outlaw name.") `applyLog` (\x -> (length x, "Applied length."))

-- hence we append things let's use Monoid type class
applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log) f = let (y, newLog) = f x in (y, log `mappend` newLog)

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

main2 = ("beans", Sum 20) `applyLog` addDrink


-- type Writer w a 

-- the Writer type
newtype Writer' w a = Writer' { runWriter' :: (a, w) }

instance (Monoid w) => Monad (Writer' w) where
    return x = Writer' (x, mempty)
    (Writer' (x, v)) >>= f = let (Writer' (y, v')) = f x in Writer' (y, v `mappend` v')

main3 = runWriter' (return 3 :: Writer' (Product Int) Int)

-- do notation with Writer
logNumber' :: Int -> Writer' [String] Int
logNumber' x = Writer' (x, ["GotNumber: " ++ show x])

multWithLog :: Writer' [String] Int
multWithLog = do
  a <- logNumber' 3
  b <- logNumber' 5
  return (a * b)

main4 = runWriter' multWithLog

-- tell function is used to add logs
-- be careful with monoid to use - lists can be very slow

-- difference lists:
-- prepending create another external lambda
-- any call calculates it all

-- \xs -> "dog" ++ ("meat" ++ xs)  


newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a  
toDiffList xs = DiffList (xs ++)  

fromDiffList :: DiffList a -> [a]  
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))




-- **** Reader 3
-- instance Monad ((->) r) where
--    return x = \_ -> x
--    h >>= f = \w -> f (h w) w

addStuff :: Int -> Int
addStuff = do
  a <- (*2)
  b <- (+10)
  return (a+b)

main5 = addStuff 3
-- (*2) >>= (\a -> (+10) >>= (\b -> return (a+b)))   applied to 3

-- (+10) >>= (\b -> return (6+b))   applied to 3
-- return (6+13)   applied to 3
-- (\_ -> 19)   applied to 3
-- 19


gcd' :: Int -> Int -> Writer [String] Int  
gcd' a b  
    | b == 0 = do  
        tell ["Finished with " ++ show a]  
        return a  
    | otherwise = do  
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
        gcd' b (a `mod` b)  

gcd'' :: Int -> Int -> Writer (DiffList String) Int  
gcd'' a b  
    | b == 0 = do  
        tell (toDiffList ["Finished with " ++ show a])  
        return a  
    | otherwise = do  
        result <- gcd'' b (a `mod` b)  
        tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])  
        return result  


finalCountDown :: Int -> Writer (DiffList String) ()  
finalCountDown 0 = do  
   tell (toDiffList ["0"])  
finalCountDown x = do  
   finalCountDown (x-1)  
   tell (toDiffList [show x])  


finalCountDown' :: Int -> Writer [String] ()  
finalCountDown' 0 = do  
   tell ["0"]  
finalCountDown' x = do  
   finalCountDown' (x-1)  
   tell [show x]  
