module Sphere
  ( Sphere(Sphere, color)
  , intersect
  , normal
  )
where
import           Vector                         ( Vector(Vector)
                                                , unitVector
                                                )
import           Point                          ( Point(Point) )
import           Math                           ( square
                                                , minroot
                                                )

import           Color

data Sphere = Sphere {
                     center :: Point,
                     radius :: Double,
                     color :: Color
                     } deriving (Show)

intersect :: Point -> Vector -> Sphere -> Maybe Point
intersect (Point (x0, y0, z0)) (Vector (xr, yr, zr)) (Sphere (Point (xc, yc, zc)) rc _)
  = makePoint <$> root
 where
  a    = square xr + square yr + square zr
  b    = 2 * ((x0 - xc) * xr + (y0 - yc) * yr + (z0 - zc) * zr)
  c    = square (x0 - xc) + square (y0 - yc) + square (z0 - zc) - square rc
  root = minroot a b c

  makePoint :: Double -> Point
  makePoint n = Point (x0 + n * xr, y0 + n * yr, z0 + n * zr)


normal :: Sphere -> Point -> Vector
normal (Sphere (Point (xc, yc, zc)) _ _) (Point (xr, yr, zr)) =
  unitVector $ Vector (xc - xr, yc - yr, zc - zr)
