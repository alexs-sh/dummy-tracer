module Point
  ( Point(Point)
  , pointToTuple
  )
where

newtype Point = Point (Double,Double,Double) deriving (Show)

pointToTuple :: Point -> (Double, Double, Double)
pointToTuple (Point (x, y, z)) = (x, y, z)
