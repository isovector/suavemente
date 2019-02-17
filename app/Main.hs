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
import Web.Suavemente.Input
import Text.Blaze (preEscapedString)

main :: IO ()
main = do
  putStrLn "Server 👍"
  putStrLn "point a client to localhost:8080"
  let t1 = textbox_ "value" "abc"
  suavementely
    [ ("test-maybe", SomeSuave (markupTest "A Suave (Maybe String)") $
          maybeInput "show" True t1)
    , ("test-text", SomeSuave (markupTest "Just a wrapped textbox") $
          div' "wrap" "" t1)
    , ("example", SomeSuave sendDiagram example)
    ]

markupTest :: (Show a) => String -> a -> Markup
markupTest comment =
  preEscapedString . (++ "</p><hr>") . ((comment ++ "<hr><p>") ++) . show

example :: Suave (QDiagram B V2 Double Any)
example = do
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

