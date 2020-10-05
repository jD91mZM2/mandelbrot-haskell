module Main where

import Lib
import Graphics.Image

main :: IO ()
main = writeImage "out.png" $ makeMandelbrot (700, 700)
