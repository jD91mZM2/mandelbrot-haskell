module Lib
    ( makeMandelbrot
    ) where

import Graphics.Image hiding (realPart, imagPart, scale)
import Graphics.Image.Interface
import Data.Complex

maxDepth = 200
scale = 200

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

-- Pixel coordinates to complex numer
coordinate :: (Int, Int) -> Complex Double
coordinate (x, y) = ((fromIntegral x) / scale) :+ ((fromIntegral y) / scale)

-- A pixel value between 0 and 1, based on how quick the blowup is
pixelValue :: (Int, Int) -> Double
pixelValue pos = (fromIntegral $ speedOfBlowup $ coordinate pos) / (fromIntegral maxDepth)

-- Make a mandelbrot image of the given size
makeMandelbrot :: (Int, Int) -> Image VS Y Double
makeMandelbrot (width, height) = makeImage (width, height) $ \(y, x) -> PixelY $ pixelValue (x - width `div` 2, y - height `div` 2)
