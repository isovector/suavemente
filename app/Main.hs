{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs  #-}
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE QuasiQuotes  #-}


module Main where

import Control.Monad.IO.Class
import Data.Functor.Identity
import Data.Proxy
import Servant
import Servant.Server
import Servant.API.WebSocket
import Servant.HTML.Blaze
import Text.Blaze (preEscapedString, Markup)
import Text.RawString.QQ

import Network.WebSockets
import Network.Wai.Handler.Warp

import Lib


main :: IO ()
main = suavemente (Slider 0 10 5 id)
-- <input type="range" min="0" max="10" value="3"/>

suavemente :: Show a => Widget a -> IO ()
suavemente w = run 8080 . serve (Proxy @API) $ htmlHandler w :<|> socketHandler w

htmlHandler :: Show a => Widget a -> Handler Markup
htmlHandler = pure . preEscapedString . mappend htmlPage . toHtml

socketHandler :: Show a => Widget a -> Connection -> Handler ()
socketHandler w c = liftIO $ do
  msg <- receive c
  print msg


htmlPage :: String
htmlPage =
  [r|
  <script>
     var ws = new WebSocket("ws://localhost:8080/suavemente");
     ws.onopen = (e) => ws.send("Suave!");
  </script>
  |]

toHtml :: Show a => Widget a -> String
toHtml (Slider l u c k) = "<input type=\"range\" min=\"" ++ show l ++ "\">"
toHtml (Checkbox b k) = "<input type=\"checkbox\" >"
toHtml (Textbox s k) = "<input type=\"text\" >"
toHtml (Dropdown os k) = "<select></select>"
toHtml (Pure a) = show a
toHtml (Apply _ _) = error "no es suavemente"

slider :: Num a => a -> a -> a -> Widget a
slider = undefined

checkbox :: Bool -> Widget Bool
checkbox = undefined

textBox :: String -> Widget String
textBox = undefined

dropDown :: [a] -> Widget a
dropDown = undefined

type API = Get '[HTML] Markup
      :<|> "suavemente" :> WebSocket

data Widget a where
  Slider   :: (Show v, Num v) => v -> v -> v -> (v -> a) -> Widget a
  Checkbox :: Bool -> (Bool -> a) -> Widget a
  Textbox  :: String -> (String -> a) -> Widget a
  Dropdown :: Show b => [b] -> (b -> a) -> Widget a
  Pure     :: a -> Widget a
  Apply    :: Widget (i -> a) -> Widget i -> Widget a

instance Functor Widget where
  -- fmap f k === f . k b/c fmap over functions is composition
  fmap f (Slider l u c k) = Slider l u c $ fmap f k
  fmap f (Checkbox b k)   = Checkbox b   $ f . k
  fmap f (Textbox s k)    = Textbox s    $ f . k
  fmap f (Dropdown os k)  = Dropdown os  $ f . k
  fmap f (Pure a)         = Pure         $ f a
  fmap f (Apply k i)      = Apply          (fmap (fmap f) k) i

instance Applicative Widget where
  pure = Pure
  (<*>) = Apply

