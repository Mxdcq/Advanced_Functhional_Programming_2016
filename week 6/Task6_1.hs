import Data.List  



solveRPN =  foldl foldingFunction ([],[]) . words 
   
foldingFunction (x:y:ys,s) "*" = ((x*y):ys , s++"This step is * ")
foldingFunction (x:y:ys,s) "+" = ((x+y):ys , s++"This step is + ")
foldingFunction (x:y:ys,s) "-" = ((y-x):ys , s++"This step is - ")
foldingFunction (x:y:ys,s) "/" = ((y/x):ys , s++"This step is / ")
foldingFunction (xs,s) numberString = (((read numberString::Float):xs) , s++"get number: "++numberString++"!")	 
 
applyLog :: (a,String) -> (a -> (b,String)) -> (b,String)  
applyLog (x,log) f = let (y,newLog) = f x in (y,log ++ newLog)



--Test
--("10 4 3 + 2 * -",""test log:") `applyLog` solveRPN 
