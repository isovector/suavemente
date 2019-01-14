module Web.Suavemente
  ( -- * Primary Stuff
    Suave ()
  , SomeSuave (..)
  , Input ()
  , suavemente
  , suavementely

    -- * Inputs
  , textbox
  , checkbox
  , slider
  , realSlider
  , dropdown
  , enumDropdown
  , colorPicker

    -- * Making New Inputs
  , mkInput
  , showMarkup

    -- * Reexports
  , Markup
  , ToMarkup (..)
  , qc
  , q
  ) where

import Web.Suavemente.Types
import Web.Suavemente.Core
import Web.Suavemente.Input
import Text.Blaze (Markup, ToMarkup (..))
import Text.InterpolatedString.Perl6 (qc, q)

