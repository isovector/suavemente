{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Suavemente.Orphans where

import           Diagrams.Backend.SVG
import qualified Diagrams.Prelude as D
import           Graphics.Svg.Core (renderBS)
import           Text.Blaze (ToMarkup (..), unsafeLazyByteString)


instance ToMarkup (D.QDiagram B D.V2 Double D.Any) where
  toMarkup = unsafeLazyByteString
           . renderBS
           . D.renderDia SVG
                         (SVGOptions (D.mkWidth 250) Nothing "" [] True)


instance (ToMarkup a, ToMarkup b) => ToMarkup (a, b) where
  toMarkup (a, b) =
    mconcat
      [ toMarkup a
      , toMarkup b
      ]


instance (ToMarkup a, ToMarkup b, ToMarkup c) => ToMarkup (a, b, c) where
  toMarkup (a, b, c) =
    mconcat
      [ toMarkup a
      , toMarkup b
      , toMarkup c
      ]


instance (ToMarkup a, ToMarkup b, ToMarkup c, ToMarkup d) => ToMarkup (a, b, c, d) where
  toMarkup (a, b, c, d) =
    mconcat
      [ toMarkup a
      , toMarkup b
      , toMarkup c
      , toMarkup d
      ]

