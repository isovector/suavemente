{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Web.Suavemente
  ( -- * Primary Stuff
    Suave ()
  , Input ()
  , suavemente

    -- * Inputs
  , textbox
  , checkbox
  , slider
  , realSlider
  , dropdown
  , enumDropdown

    -- * Making New Inputs
  , mkInput
  , showMarkup

    -- * Reexports
  , Markup
  , ToMarkup (..)
  , qc
  , q
  ) where

import           Control.Applicative (liftA2)
import           Control.Arrow ((&&&))
import           Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)
import           Control.Lens hiding ((#))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.STM (STM, atomically)
import           Control.Monad.State (StateT (..), evalStateT)
import           Control.Monad.State.Class (MonadState (..), modify)
import           Control.Monad.Trans.Class (lift)
import           Data.Bifunctor (second)
import           Data.Bool (bool)
import qualified Data.ByteString.Char8 as B
import           Data.Char (toUpper)
import           Data.Data.Lens (upon)
import           Data.Proxy (Proxy (..))
import           Diagrams.Backend.SVG (B, SVG (..), Options (..))
import qualified Diagrams.Prelude as D
import           Graphics.Svg.Core (renderBS)
import           Network.Wai.Handler.Warp (run)
import           Network.WebSockets (Connection, receiveData, sendTextData)
import           Servant (Get, Handler, (:<|>)(..), (:>), serve)
import           Servant.API.WebSocket (WebSocket)
import           Servant.HTML.Blaze (HTML)
import qualified Streaming as S
import qualified Streaming.Prelude as S
import           Text.Blaze (preEscapedString, Markup, ToMarkup (..), unsafeLazyByteString )
import           Text.Blaze.Renderer.String (renderMarkup)
import           Text.InterpolatedString.Perl6 (qc, q)


------------------------------------------------------------------------------
-- | An applicative functor capable of getting input from an HTML page.
newtype Suave a = Suave
  { suavely :: StateT Int STM (Input a)
  } deriving Functor

instance Applicative Suave where
  pure = Suave . pure . pure
  Suave f <*> Suave a = Suave $ liftA2 (<*>) f a


------------------------------------------------------------------------------
-- | An applicative functor can introduce new markup, and hook it up to the
-- event stream.
data Input a = Input
  { -- | The markup of the input.
    _iHtml :: Markup

    -- | A means of handling the event stream. The stream is of (name, value)
    -- pairs. An 'Input' is responsible for stripping its own events out of
    -- this stream.
    --
    -- The 'IO ()' action is to publish a change notification to the downstream
    -- computations.
  , _iFold :: IO ()
           -> S.Stream (S.Of (String, String)) IO ()
           -> S.Stream (S.Of (String, String)) IO ()

    -- | The current value of the 'Input'.
  , _iValue :: STM a
  } deriving Functor

instance Applicative Input where
  pure = Input mempty (const . const $ pure ()) . pure
  Input fh ff fv <*> Input ah af av =
    Input (fh <> ah)
          (liftA2 (.) ff af)
          (fv <*> av)


------------------------------------------------------------------------------
-- | Run a 'Suave' computation by spinning up its webpage at @localhost:8080@.
suavemente :: ToMarkup a => Suave a -> IO ()
suavemente w = do
  Input html f a  <- atomically $ evalStateT (suavely w) 0
  a0 <- atomically a
  run 8080
    . serve (Proxy @API)
    $ pure (htmlPage a0 <> html) :<|> socketHandler a f


------------------------------------------------------------------------------
-- | Constructor for building 'Suave' inputs that are backed by HTML elements.
mkInput
    :: Read a
    => (String -> a -> Markup)  -- ^ Function to construct the HTML element. The first parameter is what should be used for the element's 'id' attribute.
    -> a                        -- ^ The input's initial value.
    -> Suave a
mkInput f a = Suave $ do
  name <- genName
  tvar <- lift $ newTVar a
  pure $ Input (f name a) (getEvents tvar name) (readTVar tvar)


------------------------------------------------------------------------------
-- | Construct an '_iFold' field for 'Input's.
getEvents
    :: Read a
    => TVar a  -- ^ The underlying 'TVar' to publish changes to.
    -> String  -- ^ The name of the HTML input.
    -> IO ()   -- ^ Publish a change notification.
    -> S.Stream (S.Of (String, String)) IO ()
    -> S.Stream (S.Of (String, String)) IO ()
getEvents t n update
  = S.mapMaybeM (
    \a@(i, z) ->
       case i == n of
          True  -> do
            liftIO . atomically . writeTVar t . read $ z & upon head %~ toUpper
            update
            pure Nothing
          False -> pure $ Just a
           )


------------------------------------------------------------------------------
-- | Get a 'String' representation of a markup-able type. Useful for
-- constructing elements via quasiquotation.
showMarkup :: ToMarkup a => a -> String
showMarkup = renderMarkup . toMarkup


------------------------------------------------------------------------------
-- | Create an input driven by an HTML slider.
slider
    :: (ToMarkup a, Num a, Read a)
    => String  -- ^ label
    -> a       -- ^ min
    -> a       -- ^ max
    -> a       -- ^ initial value
    -> Suave a
slider label l u = mkInput $ \name v ->
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
    :: (ToMarkup a, Num a, Real a, Read a)
    => String  -- ^ label
    -> a       -- ^ min
    -> a       -- ^ max
    -> a       -- ^ step
    -> a       -- ^ initial value
    -> Suave a
realSlider label l u s = mkInput $ \name v ->
  preEscapedString
    [qc|<tr><td>
        <label for="{name}">{label}</label>
        </td><td>
        <input id="{name}" oninput="onChangeFunc(event)" type="range" min="{showMarkup l}" max="{showMarkup u}" step="{showMarkup s}" value="{showMarkup v}" autocomplete="off">
        </td></tr>|]


------------------------------------------------------------------------------
-- | Create an input driven by an HTML checkbox.
checkbox :: String -> Bool -> Suave Bool
checkbox label = mkInput $ \name v ->
  preEscapedString
    [qc|<tr><td>
        <label for="{name}">{label}</label>
        </td><td>
        <input id="{name}" oninput="onChangeBoolFunc(event)" type="checkbox" {bool ("" :: String) "checked='checked'" v} autocomplete="off">
        </td></tr>|]


------------------------------------------------------------------------------
-- | Create an input driven by an HTML textbox.
textbox
    :: String  -- ^ label
    -> String  -- ^ initial value
    -> Suave String
textbox label = mkInput $ \name v ->
  preEscapedString
    [qc|<tr><td>
        <label for="{name}">{label}</label>
        </td><td>
        <input id="{name}" oninput="onChangeStrFunc(event)" type="text" value="{v}" autocomplete="off">
        </td></tr>|]


------------------------------------------------------------------------------
-- | Create an input driven by an HTML select.
dropdown
    :: (Read a, ToMarkup a)
    => String  -- ^ label
    -> [(String, a)]
    -> a
    -> Suave a
dropdown label opts = mkInput $ \name _ -> preEscapedString $
  mconcat $
    [ [qc|<tr><td><label for="{name}">{label}</label></td><td>|]
    , [qc|<select name="{name}" onchange="onChangeFunc(event)">|]
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
    :: (Read a, ToMarkup a, Enum a, Bounded a)
    => String  -- ^ label
    -> a
    -> Suave a
enumDropdown label =
  dropdown label $ fmap (showMarkup &&& id) [minBound .. maxBound]


------------------------------------------------------------------------------
-- | HTML code to inject into all 'Suave' pages.
htmlPage :: ToMarkup a => a -> Markup
htmlPage a = preEscapedString $
  [q|
  <style>
  </style>|]
  ++
  [qc|
  <script>
     let ws = new WebSocket("ws://localhost:8080/suavemente");
     ws.onmessage = (e) => document.getElementById("result").innerHTML = e.data;
     let onChangeFunc = (e) => ws.send(e.target.id + " " + e.target.value)
     let onChangeStrFunc = (e) => ws.send(e.target.id + " \"" + e.target.value + "\"")
     let onChangeBoolFunc = (e) => ws.send(e.target.id + " " + e.target.checked)
  </script>
  <div id="result">{showMarkup a}</div>
  <table>
  |]


------------------------------------------------------------------------------
-- | 'Handler' endpoint for responding to 'Suave''s websockets.
socketHandler
    :: ToMarkup a
    => STM a
    -> (IO () -> S.Stream (S.Of (String, String)) IO () -> S.Stream (S.Of (String, String)) IO ())
    -> Connection
    -> Handler ()
socketHandler v f c
  = liftIO
  . S.effects
  . f (sendTextData c . B.pack . showMarkup =<< atomically v)
  . S.mapM (liftA2 (>>) print pure)
  . S.map (second (drop 1) . span (/= ' '))
  . S.repeatM
  . fmap B.unpack
  $ receiveData c


------------------------------------------------------------------------------
-- | Generate a new name for an HTML element.
genName :: MonadState Int m => m String
genName = do
  s <- get
  modify (+1)
  pure $ show s


------------------------------------------------------------------------------
-- | The API for 'Suave' pages.
type API = Get '[HTML] Markup
      :<|> "suavemente" :> WebSocket


------------------------------------------------------------------------------
-- | Orphan instance allowing us to draw 'D.Diagram's.
instance ToMarkup (D.QDiagram B D.V2 Double D.Any) where
  toMarkup = unsafeLazyByteString
           . renderBS
           . D.renderDia SVG
                         (SVGOptions (D.mkWidth 250) Nothing "" [] True)

