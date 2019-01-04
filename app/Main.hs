{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
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

module Main where

import           Control.Applicative
import           Control.Concurrent.STM.TVar
import           Control.Lens hiding ((#))
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Monad.State (StateT (..), evalStateT)
import           Control.Monad.State.Class
import           Control.Monad.Trans.Class
import           Data.Bifunctor
import           Data.Bool
import qualified Data.ByteString.Char8 as B
import           Data.Char
import           Data.Coerce
import qualified Data.Colour as D
import           Data.Data.Lens
import           Data.Proxy
import           Diagrams.Backend.SVG
import           Diagrams.Prelude ((#))
import qualified Diagrams.Prelude as D
import           Graphics.Svg.Core (renderBS)
import           Network.Wai.Handler.Warp
import           Network.WebSockets
import           Servant
import           Servant.API.WebSocket
import           Servant.HTML.Blaze
import qualified Streaming as S
import qualified Streaming.Prelude as S
import           Text.Blaze (preEscapedString, Markup, ToMarkup (..), unsafeLazyByteString )
import           Text.Blaze.Renderer.String
import           Text.InterpolatedString.Perl6


instance ToMarkup (D.QDiagram B D.V2 Double D.Any) where
  toMarkup = unsafeLazyByteString
           . renderBS
           . D.renderDia SVG
                         (SVGOptions (D.mkWidth 250) Nothing "" [] True)


showMarkup :: ToMarkup a => a -> String
showMarkup = renderMarkup . toMarkup

data Input a = Input
  { iHtml :: Markup
  , iFold :: IO ()
          -> S.Stream (S.Of (String, String)) IO ()
          -> S.Stream (S.Of (String, String)) IO ()
  , iValue :: STM a
  } deriving Functor


data Color = Color Int Int Int
  deriving (Eq, Ord, Show)


showColor :: Color -> Markup
showColor (Color r g b) = preEscapedString
  [qc|
    <div style="width: 200px; height: 200px; background-color: rgb({show r}, {show g}, {show b});">
      </div>
    |]


instance Applicative Input where
  pure = Input mempty (const . const $ pure ()) . pure
  Input fh ff fv <*> Input ah af av =
    Input (fh <> ah)
          (liftA2 (.) ff af)
          (fv <*> av)

newtype Suave a = Suave
  { suavely :: StateT Int STM (Input a)
  } deriving Functor

instance Applicative Suave where
  pure = Suave . pure . pure
  Suave f <*> Suave a = Suave $ liftA2 (<*>) f a


getEvents
    :: Read a
    => TVar a
    -> String
    -> IO ()
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


mkInput :: Read a => (String -> a -> Markup) -> a -> Suave a
mkInput f a = Suave $ do
  name <- genName
  tvar <- lift $ newTVar a
  pure $ Input (f name a) (getEvents tvar name) (readTVar tvar)


slider :: (ToMarkup a, Num a, Read a) => a -> a -> a -> Suave a
slider l u = mkInput $ \name v ->
  preEscapedString
    [qc|<input id="{name}" oninput="onChangeFunc(event)" type="range" min="{showMarkup l}" max="{showMarkup u}" value="{showMarkup v}"><br/>|]

realSlider :: (ToMarkup a, Num a, Real a, Read a) => a -> a -> a -> a -> Suave a
realSlider l u s = mkInput $ \name v ->
  preEscapedString
    [qc|<input id="{name}" oninput="onChangeFunc(event)" type="range" min="{showMarkup l}" max="{showMarkup u}" step="{showMarkup s}" value="{showMarkup v}"><br/>|]


checkbox :: Bool -> Suave Bool
checkbox = mkInput $ \name v ->
  preEscapedString
    [qc|<input id="{name}" oninput="onChangeBoolFunc(event)" type="checkbox" {bool "" "checked='checked'" v}><br/>|]

textbox :: String -> Suave String
textbox = mkInput $ \name v ->
  preEscapedString
    [qc|<input id="{name}" oninput="onChangeStrFunc(event)" type="text" value="{v}"><br/>|]


main :: IO ()
main = suavemente $ do
  rad <- slider 1 10 5
  r <- realSlider 0 1 0.05 1
  g <- realSlider 0 1 0.05 1
  b <- realSlider 0 1 0.05 1
  x <- slider 0 255 255
  y <- slider 0 255 255
  pure (D.circle rad # D.fc (D.sRGB r g b) # D.translate (D.r2 (x, y)) :: D.Diagram B)

suavemente :: ToMarkup a => Suave a -> IO ()
suavemente w = do
  (Input html f a)  <- atomically $ evalStateT (suavely w) 0
  a0 <- atomically a
  run 8080
    . serve (Proxy @API)
    $ pure (htmlPage a0 <> html) :<|> socketHandler a f


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
  . S.map (second tail . span (/= ' '))
  . S.repeatM
  . fmap B.unpack
  $ receiveData c


htmlPage :: ToMarkup a => a -> Markup
htmlPage a = preEscapedString
  [qc|
  <script>
     let ws = new WebSocket("ws://localhost:8080/suavemente");
     ws.onmessage = (e) => document.getElementById("result").innerHTML = e.data;
     let onChangeFunc = (e) => ws.send(e.target.id + " " + e.target.value)
     let onChangeStrFunc = (e) => ws.send(e.target.id + " \"" + e.target.value + "\"")
     let onChangeBoolFunc = (e) => ws.send(e.target.id + " " + e.target.checked)
  </script>
  <div id="result">{showMarkup a}</div>
  |]


genName :: MonadState Int m => m String
genName = do
  s <- get
  modify (+1)
  pure $ show s


makeId :: String -> String
makeId i = "id=\"" ++ i ++ "\""


type API = Get '[HTML] Markup
      :<|> "suavemente" :> WebSocket

