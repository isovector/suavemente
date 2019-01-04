{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Main where

import           Control.Applicative
import           Control.Concurrent.STM.TVar
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Monad.State (StateT (..), evalStateT)
import           Control.Monad.State.Class
import           Control.Monad.Trans.Class
import           Data.Bifunctor
import           Data.Bool
import qualified Data.ByteString.Char8 as B
import           Data.Char
import           Data.Data.Lens
import           Data.Proxy
import           Network.Wai.Handler.Warp
import           Network.WebSockets
import           Servant
import           Servant.API.WebSocket
import           Servant.HTML.Blaze
import qualified Streaming as S
import qualified Streaming.Prelude as S
import           Text.Blaze (preEscapedString, Markup)
import           Text.Blaze.Renderer.String
import           Text.InterpolatedString.Perl6


instance Show Markup where
  show = renderMarkup

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


slider :: (Show a, Num a, Read a) => a -> a -> a -> Suave a
slider l u = mkInput $ \name v ->
  preEscapedString
    [qc|<input id="{name}" oninput="onChangeFunc(event)" type="range" min="{l}" max="{u}" value="{v}"><br/>|]


checkbox :: Bool -> Suave Bool
checkbox = mkInput $ \name v ->
  preEscapedString
    [qc|<input id="{name}" oninput="onChangeBoolFunc(event)" type="checkbox" {bool "" "checked='checked'" v}><br/>|]

textbox :: String -> Suave String
textbox = mkInput $ \name v ->
  preEscapedString
    [qc|<input id="{name}" oninput="onChangeStrFunc(event)" type="text" value="{v}"><br/>|]


main :: IO ()
main = suavemente
     . fmap showColor
     $ Color <$> slider 0 255 255
             <*> slider 0 255 255
             <*> slider 0 255 255

suavemente :: Show a => Suave a -> IO ()
suavemente w = do
  (Input html f a)  <- atomically $ evalStateT (suavely w) 0
  a0 <- atomically a
  run 8080
    . serve (Proxy @API)
    $ pure (htmlPage a0 <> html) :<|> socketHandler a f


socketHandler
    :: Show a
    => STM a
    -> (IO () -> S.Stream (S.Of (String, String)) IO () -> S.Stream (S.Of (String, String)) IO ())
    -> Connection
    -> Handler ()
socketHandler v f c
  = liftIO
  . S.effects
  . f (sendTextData c . B.pack . show =<< atomically v)
  . S.mapM (liftA2 (>>) print pure)
  . S.map (second tail . span (/= ' '))
  . S.repeatM
  . fmap B.unpack
  $ receiveData c


htmlPage :: Show a => a -> Markup
htmlPage a = preEscapedString
  [qc|
  <script>
     let ws = new WebSocket("ws://localhost:8080/suavemente");
     ws.onmessage = (e) => document.getElementById("result").innerHTML = e.data;
     let onChangeFunc = (e) => ws.send(e.target.id + " " + e.target.value)
     let onChangeStrFunc = (e) => ws.send(e.target.id + " \"" + e.target.value + "\"")
     let onChangeBoolFunc = (e) => ws.send(e.target.id + " " + e.target.checked)
  </script>
  <div id="result">{show a}</div>
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

