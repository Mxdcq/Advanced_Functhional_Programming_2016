import Data.List
import Control.Monad
import Control.Applicative

rPN :: String -> Maybe Float  
rPN = head . foldl foldingFunction [] . words  
   where   foldingFunction (x:y:ys) "*" = ((*) <$> x <*> y):ys  
           foldingFunction (x:y:ys) "+" = ((+) <$> x <*> y):ys  
           foldingFunction (x:y:ys) "-" = ((-) <$> y <*> x):ys
           foldingFunction ((Just 0):y:ys) "/" = Nothing:ys  
           foldingFunction (x:y:ys) "/" = ((/) <$> y <*> x):ys  
           foldingFunction xs numberString = (Just (read numberString::Float)):xs