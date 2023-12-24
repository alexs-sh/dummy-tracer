module Math
  ( square
  , minroot
  , magnitude
  , distance
  )
where

square :: Double -> Double
square a = a * a

minroot :: Double -> Double -> Double -> Maybe Double
minroot a b c | a == 0    = linear
              | otherwise = quadratic
 where
  linear :: Maybe Double
  linear | b == 0    = Nothing
         | otherwise = Just ((-c) / b)
  quadratic :: Maybe Double
  quadratic
    | d < 0
    = Nothing
    | otherwise
    = let sd = sqrt d
          aa = 2 * a
      in  Just (min ((-b + sd) / aa) ((-b - sd) / aa))
    where d = square b - 4 * a * c

magnitude :: (Double, Double, Double) -> Double
magnitude (ax, ay, az) = sqrt (square ax + square ay + square az)

distance :: (Double, Double, Double) -> (Double, Double, Double) -> Double
distance (x1, y1, z1) (x2, y2, z2) =
  magnitude ((x1 - x2), (y1 - y2), (z1 - z2))
