{-# LANGUAGE ApplicativeDo    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE TypeFamilies     #-}

module Main where

import Data.Bool
import Diagrams.Backend.SVG
import Diagrams.Prelude hiding (rad)
import Web.Suavemente


main :: IO ()
main = suavementely
  [ ("zoom", SomeSuave $ do
      sq  <- dropdown "Shape"
               [ ("Triangle", "0")
               , ("Square", "1")
               , ("Circle", "2")
               , ("Star", "3")
               ] ("0" :: String)
      rad <- slider "Radius" 1 10 5
      n   <- slider "Polys" 5 12 5
      r   <- realSlider "Red" 0 1 0.05 0.5
      g   <- realSlider "Green" 0 1 0.05 1
      b   <- realSlider "Blue" 0 1 0.05 1
      x   <- slider "X" 0 20 10
      y   <- slider "Y" 0 20 10

      pure (
        getShape sq rad n
                # fc (sRGB r g b)
                # translate (r2 (x, y))
                # rectEnvelope (p2 (0, 0)) (r2 (20, 20))
        :: Diagram B)
      )

  , ("fwop", SomeSuave $ do
      bool <- checkbox "Check" False
      string <- textbox "String" "hello"

      pure (bool, string)
    )
  ]


getShape :: String -> Double -> Int -> Diagram B
getShape "0" rad z = triangle $ rad * 2
getShape "1" rad z = square $ rad * 2
getShape "2" rad z = circle rad
getShape "3" rad z = star (StarSkip 2) (regPoly z rad) # strokeP # lw none

