--在模块的开头定义模块的名称，如果文件名叫做Geometry.hs那它的名字就得是Geometry。在声明出它含有的函数名之后就可以编写函数的实现啦，就这样写:
module Geometry   
( sphereVolume   
,sphereArea   
,cubeVolume   
,cubeArea   
,cuboidArea   
,cuboidVolume   
) where

--继续进发，定义函数体:
sphereVolume :: Float -> Float   
sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)   
 
sphereArea :: Float -> Float   
sphereArea radius = 4 * pi * (radius ^ 2)   
 
cubeVolume :: Float -> Float   
cubeVolume side = cuboidVolume side side side   
 
cubeArea :: Float -> Float   
cubeArea side = cuboidArea side side side   
 
cuboidVolume :: Float -> Float -> Float -> Float   
cuboidVolume a b c = rectangleArea a b * c   
 
cuboidArea :: Float -> Float -> Float -> Float   
cuboidArea a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2   
 
rectangleArea :: Float -> Float -> Float   
rectangleArea a b = a * b