module Vector
  ( unitVector
  , vectorToTuple
  , Vector(Vector)
  )
where

import           Math                           ( magnitude )

newtype Vector = Vector (Double,Double,Double) deriving (Show)

vectorToTuple :: Vector -> (Double, Double, Double)
vectorToTuple (Vector (x, y, z)) = (x, y, z)

unitVector :: Vector -> Vector
unitVector vector@(Vector (x, y, z)) = Vector (x / d, y / d, z / d)
  where d = magnitude $ vectorToTuple vector
