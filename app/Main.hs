{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs  #-}
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE QuasiQuotes  #-}


module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class
import Data.Functor.Identity
import qualified Data.Map as M
import Data.Foldable
import Data.Proxy
import Servant
import Servant.Server
import Servant.API.WebSocket
import Servant.HTML.Blaze
import Text.Blaze (preEscapedString, Markup)
import Text.RawString.QQ
import Text.InterpolatedString.Perl6

import Network.WebSockets
import Network.Wai.Handler.Warp

import Lib


main :: IO ()
main = suavemente (Slider 0 10 5 id)
-- <input type="range" min="0" max="10" value="3"/>

suavemente :: Show a => Widget a -> IO ()
suavemente w = run 8080 . serve (Proxy @API) $ htmlHandler w :<|> socketHandler w

htmlHandler :: Show a => Widget a -> Handler Markup
htmlHandler = pure . preEscapedString . mappend htmlPage . concat . toList . flip evalState 0 . toHtml

socketHandler :: Show a => Widget a -> Connection -> Handler ()
socketHandler w c = liftIO $ forever $ do
  msg <- receive c
  print msg


htmlPage :: String
htmlPage =
  [r|
  <script>
     let ws = new WebSocket("ws://localhost:8080/suavemente");
     ws.onopen = (e) => ws.send("Suave!");
     let onChangeFunc = (e) => ws.send(`Changed: ${e.target.value} Id: ${e.target.id}`)
  </script>
  |]

-- { sideLength: Int
-- , color: Color
-- }

genName :: State Int String
genName = do
  state <- get
  modify (+1)
  pure $ show state

makeId :: String -> String
makeId i = "id=\"" ++ i ++ "\""

toHtml :: Widget a -> State Int (M.Map String String)
toHtml (Slider l u c k) = do
  name <- genName
  pure $ M.singleton name [qc|<input id="{name}" onchange="onChangeFunc(event)" type="range" min="{show l}">|]
toHtml (Checkbox b k)   = do
  name <- genName
  pure $ M.singleton name  [qc|<input id="{name}" type="checkbox" >|]
toHtml (Textbox s k)    = do
  name <- genName
  pure $ M.singleton name  [qc|<input id="{name}" type="text" >|]
toHtml (Dropdown os k)  = do
  name <- genName
  pure $ M.singleton name  [qc|<select id="{name}"></select>|]
toHtml (Pure a)         = do
  name <- genName
  pure $ M.singleton name [qc|<div id={name}></div>|]
toHtml (Apply f a)      = do
  liftA2 mappend (toHtml f) (toHtml a)

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

-- @TODO
-- assign IDs
--
