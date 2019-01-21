# suavemente

[![Build Status](https://travis-ci.org/isovector/suavemente.svg?branch=master)][build]
[![Hackage](https://img.shields.io/hackage/v/suavemente.svg)][hackage]
[![Stackage LTS](https://www.stackage.org/package/suavemente/badge/lts)][stackage]
[![Stackage Nightly](https://www.stackage.org/package/suavemente/badge/nightly)][nightly]

[build]: https://travis-ci.org/isovector/suavemente
[hackage]: https://hackage.haskell.org/package/suavemente
[stackage]: http://stackage.org/lts/package/suavemente
[nightly]: http://stackage.org/nightly/package/suavemente

## Dedication

> Today's kitchen is all about a well-planned space that makes cooking a
> completely interactive experience among family and friends.
>
> Candice Olson


## Overview

Suavemente is an applicative functor capable of seamlessly talking to HTML
elements. Running a suavemente program automatically spins up a webserver and
hooks up its pages with websockets. The use case is to quickly deploy simple,
interactive Haskell programs without needing to figure out how the fuck GHCJS
works.


## Example

```haskell
{-# LANGUAGE ApplicativeDo #-}

module Main where

import Diagrams.Backend.SVG
import Diagrams.Prelude hiding (rad)
import Web.Suavemente
import Web.Suavemente.Diagrams


main :: IO ()
main = suavemente sendDiagram $ do
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
```

Hit `localhost:8080` to see it in action!

