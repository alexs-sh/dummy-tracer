module Main
  ( main
  )
where

import           System.IO

import           Vector                         ( Vector(Vector)
                                                , unitVector
                                                , vectorToTuple
                                                )
import           Sphere                         ( Sphere(Sphere)
                                                , intersect
                                                , normal
                                                , color
                                                )
import           Point                          ( Point(Point)
                                                , pointToTuple
                                                )

import           Color                          ( Color(RGB, Grayscale)
                                                , mulColor
                                                --, getGrayscale
                                                )

world :: [Sphere]
world =
  [ Sphere (Point (0, 0, -400))       300 (RGB (0.2, 1.0, 0.0))
  , Sphere (Point (300, -300, -100))  80  (RGB (1.0, 0.0, 0.8))
  , Sphere (Point (-300, -300, -100)) 80  (RGB (0.0, 1.0, 0.8))
  , Sphere (Point (-0, -300, -100))   80  (RGB (1.0, 0.8, 0.8))
  , Sphere (Point (300, 300, -100))   80  (RGB (0.8, 0.0, 1.0))
  , Sphere (Point (-300, 300, -100))  80  (RGB (0.0, 0.8, 1.0))
  , Sphere (Point (-0, 300, -100))    80  (RGB (1.0, 0.8, 0.0))
  ]

eye :: Point
eye = Point (0, 0, 400)

trace :: [Color]
trace = [ colorAt (x, y) | y <- [-500 .. 499], x <- [-500 .. 499] ]

lambert :: Sphere -> Point -> Vector -> Color
lambert sphere point (Vector (xr, yr, zr)) = mulColor (color sphere) k
 where
  k            = max 0.08 (xr * xn + yr * yn + zr * zn)
  (xn, yn, zn) = vectorToTuple $ normal sphere point

hitAt :: (Double, Double) -> Maybe (Sphere, Vector, Point)
hitAt (x, y) = foldr hitWith Nothing world
 where
  (eyex, eyey, eyez) = pointToTuple eye
  ray                = unitVector $ Vector (x - eyex, y - eyey, negate eyez)
  hitWith
    :: Sphere -> Maybe (Sphere, Vector, Point) -> Maybe (Sphere, Vector, Point)
  hitWith _ prev@(Just _) = prev
  hitWith sphere Nothing =
    (\point -> (sphere, ray, point)) <$> intersect eye ray sphere

colorAt :: (Double, Double) -> Color
colorAt (ax, ay) = case hitAt (ax, ay) of
  Just (sphere, ray, point) -> mulColor (lambert sphere point ray) 255.0
  Nothing                   -> RGB (0, 0, 0)

--writeP2Data :: [Color] -> Handle -> IO ()
--writeP2Data [] _    = return ()
--writeP2Data xs file = do
--  mapM_ (\x -> hPutStr file (show ((round . getGrayscale) x) ++ " "))
--    $ take 100 xs
--  hPutStrLn file ""
--  writeP2Data (drop 100 xs) file
--
--saveP2 :: String -> [Color] -> IO ()
--saveP2 path pixels = do
--  file <- openFile path System.IO.WriteMode
--  hPutStrLn file "P2 1000 1000 255"
--  writeP2Data pixels file
--  hClose file

writeP3Data :: [Color] -> Handle -> IO ()
writeP3Data [] _    = return ()
writeP3Data xs file = do
  mapM_
      ( (\(r, g, b) ->
          hPutStr file (show r ++ " " ++ show g ++ " " ++ show b ++ " ")
        )
      . normalize
      )
    $ take 100 xs
  hPutStrLn file ""
  writeP3Data (drop 100 xs) file
 where
  normalize (RGB       (r, g, b)) = (round r, round g, round b)
  normalize (Grayscale _        ) = undefined


saveP3 :: String -> [Color] -> IO ()
saveP3 path pixels = do
  file <- openFile path System.IO.WriteMode
  hPutStrLn file "P3 1000 1000 255"
  writeP3Data pixels file
  hClose file


main :: IO ()
main = do
  --saveP2 "/tmp/trace2P.pic" pixels
  saveP3 "/tmp/trace3P.pic" trace
