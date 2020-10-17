module Main where

import Lib
import Graphics.Image (writeImage)

main :: IO ()
main = writeImage "out.png" $ drawMandelbrot $ (makeMandelbrot 700 700) { scale = 0.95 }
