module Lib
    ( Mandelbrot(..)
    , makeMandelbrot
    , drawMandelbrot
    ) where

import Graphics.Image hiding (realPart, imagPart, scale)
import Data.Complex

maxDepth :: Int
maxDepth = 200

-- The function to test on
f :: Complex Double -> Complex Double -> Complex Double
f z c = z ** 2 + c

-- Return infinite list of all numbers in sequence
nextValues :: Complex Double -> Complex Double -> [Complex Double]
nextValues z c = z:nextValues (f z c) c

series :: Complex Double -> [Complex Double]
series pos = nextValues (0 :+ 0) pos

-- See if number is within radius of convergence (using the pythagorean theorem)
mightConverge :: Complex Double -> Bool
mightConverge z = (realPart z) ** 2 + (imagPart z) ** 2 <= 2 ** 2

-- Count how quickly the infinite list becomes guaranteed to never converge
speedOfBlowup :: Complex Double -> Int
speedOfBlowup pos = length $ takeWhile mightConverge $ take maxDepth $ series pos

-- A color value between 0 and 1, based on how quick the blowup is
valueOf :: Complex Double -> Double
valueOf pos = (fromIntegral $ speedOfBlowup pos) / (fromIntegral maxDepth)

-- Mandelbrot settings
data Mandelbrot = Mandelbrot
  { width :: Int
  , height :: Int
  , scale :: Double
  , x_offset :: Double
  , y_offset :: Double
  }

makeMandelbrot :: Int -> Int -> Mandelbrot
makeMandelbrot w h = Mandelbrot
  { width = w
  , height = h
  , scale = 1
  , x_offset = 0
  , y_offset = 0
  }

-- Pixel coordinates to complex numer
coordinate :: Mandelbrot -> (Int, Int) -> Complex Double
coordinate m (x, y) =
  let
    x_proportional = fromIntegral x / fromIntegral (width m)
    y_proportional = fromIntegral y / fromIntegral (height m)

    -- Instead of going from 0 to 1, go from -2 to 2
    x_normalised = (x_proportional - 0.5) * (2 / 0.5)
    y_normalised = (y_proportional - 0.5) * (2 / 0.5)
  in
    ((x_normalised - x_offset m) / scale m) :+ ((y_normalised - y_offset m) / scale m)

-- Make a mandelbrot image of the given size and zoom
drawMandelbrot :: Mandelbrot -> Image VS Y Double
drawMandelbrot m = makeImage (width m, height m) $ \(y, x) -> PixelY $ valueOf $ coordinate m (x, y)
