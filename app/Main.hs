{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Main where

import           Control.Applicative
import           Control.Concurrent.STM.TVar
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Monad.State (StateT (..), evalStateT)
import           Control.Monad.State.Class
import           Control.Monad.Trans.Class
import           Data.Bifunctor
import qualified Data.ByteString.Char8 as B
import           Data.Proxy
import           Network.Wai.Handler.Warp
import           Network.WebSockets
import           Servant
import           Servant.API.WebSocket
import           Servant.HTML.Blaze
import qualified Streaming as S
import qualified Streaming.Prelude as S
import           Text.Blaze (preEscapedString, Markup)
import           Text.InterpolatedString.Perl6


data Input a = Input
  { iHtml :: Markup
  , iFold :: IO ()
          -> S.Stream (S.Of (String, String)) IO ()
          -> S.Stream (S.Of (String, String)) IO ()
  , iValue :: STM a
  } deriving Functor



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
            liftIO $ atomically $ writeTVar t $ read z
            update
            pure Nothing
          False -> pure $ Just a
           )


slider :: (Show a, Num a, Read a) => a -> a -> a -> Suave a
slider l u v = Suave $ do
  name <- genName
  let mu = [qc|<input id="{name}" onchange="onChangeFunc(event)" type="range" min="{l}" max="{u}" value="{v}"><br/>|]
  tvar <- lift $ newTVar v
  pure $ Input (preEscapedString mu) (getEvents tvar name) (readTVar tvar)


main :: IO ()
main = suavemente $ liftA2 (+) (slider @Int 0 10 5) (slider 0 10 5)

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
  . S.map (second tail . span (/= ' '))
  . S.repeatM
  . fmap B.unpack
  $ receiveData c


htmlPage :: Show a => a -> Markup
htmlPage a = preEscapedString
  [qc|
  <script>
     let ws = new WebSocket("ws://localhost:8080/suavemente");
     ws.onopen = (e) => ws.send("Suave!");
     ws.onmessage = (e) => document.getElementById("result").innerHTML = e.data;
     let onChangeFunc = (e) => ws.send(e.target.id + " " + e.target.value)
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


-- toHtml :: Widget a -> State Int (M.Map String String)
-- toHtml (Slider l u c k) = do
--   name <- genName
--   pure $ M.singleton name
--     [qc|<input id="{name}" onchange="onChangeFunc(event)" type="range" min="{show l}">|]
-- toHtml (Checkbox b k)   = do
--   name <- genName
--   pure $ M.singleton name
--     [qc|<input id="{name}" type="checkbox" >|]
-- toHtml (Textbox s k)    = do
--   name <- genName
--   pure $ M.singleton name
--     [qc|<input id="{name}" type="text" >|]
-- toHtml (Dropdown os k)  = do
--   name <- genName
--   pure $ M.singleton name
--     [qc|<select id="{name}"></select>|]
-- toHtml (Pure a)         = do
--   name <- genName
--   pure $ M.singleton name
--     [qc|<div id={name}></div>|]
-- toHtml (Apply f a)      = do
--   liftA2 mappend (toHtml f) (toHtml a)


type API = Get '[HTML] Markup
      :<|> "suavemente" :> WebSocket

