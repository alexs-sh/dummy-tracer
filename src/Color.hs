module Color
  ( Color(RGB, Grayscale)
  , mulColor
  , getGrayscale
  )
where


data Color = RGB (Double,Double,Double) | Grayscale Double deriving (Show)

getGrayscale :: Color -> Double
getGrayscale (RGB       (r, g, b)) = r + g + b / 3
getGrayscale (Grayscale g        ) = g

mulColor :: Color -> Double -> Color
mulColor (RGB       (r, g, b)) n = RGB (r * n, g * n, b * n)
mulColor (Grayscale g        ) n = Grayscale (g * n)
