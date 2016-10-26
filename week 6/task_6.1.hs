import Control.Monad.Writer
import Data.List

solveRPN =  foldl foldingFunction ([],[]) . words 
   
foldingFunction (x:y:ys,s) "+" = writer ((x+y):ys , [(show s)++"This step is + "])
foldingFunction (x:y:ys,s) "-" = writer ((y-x):ys , [(show s)++"This step is - "])
foldingFunction (x:y:ys,s) "*" = writer ((x*y):ys , [(show s)++"This step is * "])
foldingFunction (x:y:ys,s) "/" = writer ((y/x):ys , [(show s)++"This step is / "])
foldingFunction (xs,s) numberString = writer ((read numberString:xs) , [(show s)++"get number: "++numberString++"!"])

