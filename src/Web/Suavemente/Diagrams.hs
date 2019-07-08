{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Suavemente.Diagrams where

import           Diagrams.Backend.SVG
import qualified Diagrams.Prelude as D
import           Graphics.Svg.Core (renderBS)
import           Text.Blaze (Markup, unsafeLazyByteString)


sendDiagram :: Double -> D.Diagram B -> Markup
sendDiagram w
  = unsafeLazyByteString
  . renderBS
  . D.renderDia SVG
                (SVGOptions (D.mkWidth w) Nothing "" [] True)

