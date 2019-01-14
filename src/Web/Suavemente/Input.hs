{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}

module Web.Suavemente.Input where

import           Control.Arrow ((&&&))
import           Control.Concurrent.STM.TVar (newTVar, readTVar)
import           Control.Monad.State.Class (MonadState (..), modify)
import           Control.Monad.Trans.Class (lift)
import           Data.Aeson (FromJSON (..), Value, withText)
import           Data.Aeson.Types (Parser)
import           Data.Bool (bool)
import           Data.Colour.SRGB (Colour, sRGB24show, sRGB24read)
import           Data.Text (unpack)
import           Text.Blaze (preEscapedString, Markup, ToMarkup (..))
import           Text.InterpolatedString.Perl6 (qc, q)
import           Web.Suavemente.Core
import           Web.Suavemente.Types


------------------------------------------------------------------------------
-- | Generate a new name for an HTML element.
genName :: MonadState Int m => m String
genName = do
  s <- get
  modify (+1)
  pure $ show s


------------------------------------------------------------------------------
-- | Constructor for building 'Suave' inputs that are backed by HTML elements.
mkInput
    :: (Value -> Parser a)
    -> (String -> a -> Markup)  -- ^ Function to construct the HTML element. The first parameter is what should be used for the element's 'id' attribute.
    -> a                        -- ^ The input's initial value.
    -> Suave a
mkInput p f a = Suave $ do
  name <- genName
  tvar <- lift $ newTVar a
  pure $ Input (f name a) (getEvents p tvar name) (readTVar tvar)


------------------------------------------------------------------------------
-- | Create an input driven by an HTML slider.
slider
    :: (ToMarkup a, Num a, FromJSON a)
    => String  -- ^ label
    -> a       -- ^ min
    -> a       -- ^ max
    -> a       -- ^ initial value
    -> Suave a
slider label l u = mkInput parseJSON $ \name v ->
  preEscapedString
    [qc|<tr><td>
        <label for="{name}">{label}</label>
        </td><td>
        <input id="{name}" oninput="onChangeFunc(event)" type="range" min="{showMarkup l}" max="{showMarkup u}" value="{showMarkup v}" autocomplete="off">
        </td></tr>|]


------------------------------------------------------------------------------
-- | Create an input driven by an HTML slider, whose domain is the real
-- numbers.
realSlider
    :: (ToMarkup a, Num a, Real a, FromJSON a)
    => String  -- ^ label
    -> a       -- ^ min
    -> a       -- ^ max
    -> a       -- ^ step
    -> a       -- ^ initial value
    -> Suave a
realSlider label l u s = mkInput parseJSON $ \name v ->
  preEscapedString
    [qc|<tr><td>
        <label for="{name}">{label}</label>
        </td><td>
        <input id="{name}" oninput="onChangeFunc(event)" type="range" min="{showMarkup l}" max="{showMarkup u}" step="{showMarkup s}" value="{showMarkup v}" autocomplete="off">
        </td></tr>|]


------------------------------------------------------------------------------
-- | Create an input driven by the HTML input, type=color.
colorPicker
    :: (Ord a, Floating a, RealFrac a)
    => String   -- ^ label
    -> Colour a -- ^ initial value
    -> Suave (Colour a)
colorPicker label =
  mkInput
  (withText "hex colour representation" (pure . sRGB24read . unpack)) $
  \name v ->
    preEscapedString
    [qc|<tr><td>
        <label for="{name}">{label}</label>
        </td><td>
        <input id="{name}" oninput="onChangeFunc(event)" type="color" value="{sRGB24show v}">
        </td></tr>|]

------------------------------------------------------------------------------
-- | Create an input driven by an HTML checkbox.
checkbox :: String -> Bool -> Suave Bool
checkbox label = mkInput parseJSON $ \name v ->
  preEscapedString
    [qc|<tr><td>
        <label for="{name}">{label}</label>
        </td><td>
        <input id="{name}" onchange="onChangeFunc(event)" type="checkbox" {bool ("" :: String) "checked='checked'" v} autocomplete="off">
        </td></tr>|]


------------------------------------------------------------------------------
-- | Create an input driven by an HTML textbox.
textbox
    :: String  -- ^ label
    -> String  -- ^ initial value
    -> Suave String
textbox label = mkInput parseJSON $ \name v ->
  preEscapedString
    [qc|<tr><td>
        <label for="{name}">{label}</label>
        </td><td>
        <input id="{name}" oninput="onChangeFunc(event)" type="text" value="{v}" autocomplete="off">
        </td></tr>|]


------------------------------------------------------------------------------
-- | Create an input driven by an HTML select.
dropdown
    :: (FromJSON a, ToMarkup a)
    => String  -- ^ label
    -> [(String, a)]
    -> a
    -> Suave a
dropdown label opts = mkInput parseJSON $ \name _ -> preEscapedString $
  mconcat $
    [ [qc|<tr><td><label for="{name}">{label}</label></td><td>|]
    , [qc|<select id="{name}" onchange="onChangeFunc(event)" autocomplete="off">|]
    ] ++
    fmap (\(oname, oval) -> [qc|<option value="{showMarkup oval}">{oname}</option>|])
         opts
      ++
    [ [q|</select>|]
    , [q|</td></tr>|]
    ]


------------------------------------------------------------------------------
-- | Create an input for enums driven by an HTML select.
enumDropdown
    :: (FromJSON a, ToMarkup a, Enum a, Bounded a)
    => String  -- ^ label
    -> a
    -> Suave a
enumDropdown label =
  dropdown label $ fmap (showMarkup &&& id) [minBound .. maxBound]

