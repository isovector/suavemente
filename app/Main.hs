{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wall      #-}

module Main where

import Diagrams.Backend.SVG
import Diagrams.Prelude hiding (rad)
import Lib


main :: IO ()
main = suavemente $ do
  rad <- slider "Radius" 1 10 5
  r   <- realSlider "Red" 0 1 0.05 1
  g   <- realSlider "Green" 0 1 0.05 1
  b   <- realSlider "Blue" 0 1 0.05 1
  x   <- slider "X" 0 20 10
  y   <- slider "Y" 0 20 10

  pure (
    circle rad
            # fc (sRGB r g b)
            # translate (r2 (x, y))
            # rectEnvelope (p2 (0, 0)) (r2 (20, 20))
    :: Diagram B)

