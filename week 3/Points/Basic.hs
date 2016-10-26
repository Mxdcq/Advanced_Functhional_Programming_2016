module Points.Basic
( Point (..)
, Distance (..)
, manh ) where

data Point = Point Float Float deriving (Show)
type Distance = Float

manh :: Point -> Point -> Distance
manh (Point x1 y1) (Point x2 y2) = (abs (x1-x2)) + (abs (y1-y2))


