{-# LANGUAGE ApplicativeDo    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE TypeFamilies     #-}

module Main where

import Data.Bool
import Diagrams.Backend.SVG
import Diagrams.Prelude hiding (rad)
import Web.Suavemente
import Web.Suavemente.Diagrams

main :: IO ()
main = suavemente sendDiagram $ do
  sq  <- dropdown "Shape"
           [ ("Triangle", "0")
           , ("Square", "1")
           , ("Circle", "2")
           , ("Star", "3")
           ] ("0" :: String)
  rad <- realSlider "Radius" 1 10 0.1 5
  n   <- slider "Polys" 5 12 5
  c   <- colorPicker "Color" (sRGB 0.5 0.15 0.3)
  x   <- slider "X" 0 20 10
  y   <- slider "Y" 0 20 10

  pure (
    getShape sq rad n
            # fc c
            # translate (r2 (x, y))
            # rectEnvelope (p2 (0, 0)) (r2 (20, 20))
    :: Diagram B)

getShape :: String -> Double -> Int -> Diagram B
getShape "0" rad z = triangle $ rad * 2
getShape "1" rad z = square $ rad * 2
getShape "2" rad z = circle rad
getShape "3" rad z = star (StarSkip 2) (regPoly z rad) # strokeP # lw none

