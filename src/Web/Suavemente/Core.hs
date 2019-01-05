{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Web.Suavemente.Core where

import           Control.Applicative (liftA2)
import           Control.Concurrent.STM.TVar (TVar, writeTVar)
import           Control.Monad.Except (throwError)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.STM (atomically)
import           Control.Monad.State (evalStateT)
import           Data.Aeson (FromJSON (..), decode, fromJSON, Result (..))
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import           Data.Proxy (Proxy (..))
import           Network.Wai.Handler.Warp (run)
import           Network.WebSockets (Connection, receiveData, sendTextData)
import           Servant (Get, Handler, Capture, (:<|>)(..), (:>), serve, err404)
import           Servant.API.WebSocket (WebSocket)
import           Servant.HTML.Blaze (HTML)
import qualified Streaming as S
import qualified Streaming.Prelude as S
import           Text.Blaze (preEscapedString, Markup, ToMarkup (..))
import           Text.Blaze.Renderer.String (renderMarkup)
import           Text.InterpolatedString.Perl6 (qc, q)
import           Web.Suavemente.Types


------------------------------------------------------------------------------
-- | Get a 'String' representation of a markup-able type. Useful for
-- constructing elements via quasiquotation.
showMarkup :: ToMarkup a => a -> String
showMarkup = renderMarkup . toMarkup


------------------------------------------------------------------------------
-- | EXPLODE IF PARSING FAILS
fromResult :: Result a -> a
fromResult (Success a) = a
fromResult (Error s) = error s


------------------------------------------------------------------------------
-- | Construct an '_iFold' field for 'Input's.
getEvents
    :: FromJSON a
    => TVar a  -- ^ The underlying 'TVar' to publish changes to.
    -> String  -- ^ The name of the HTML input.
    -> IO ()   -- ^ Publish a change notification.
    -> S.Stream (S.Of ChangeEvent) IO ()
    -> S.Stream (S.Of ChangeEvent) IO ()
getEvents t n update
  = S.mapMaybeM (
    \a@(ChangeEvent i z) ->
       case i == n of
          True  -> do
            liftIO . atomically . writeTVar t . fromResult $ fromJSON z
            update
            pure Nothing
          False -> pure $ Just a
           )


------------------------------------------------------------------------------
-- | HTML code to inject into all 'Suave' pages.
htmlPage :: ToMarkup a => String -> a -> Markup
htmlPage res a = preEscapedString $
  [q|
  <style>
  </style>|]
  ++
  [q|
  <script>
    // from https://code.lengstorf.com/get-form-values-as-json/
    const isCheckbox = e => e.type === 'checkbox';
    const isMultiSelect = e => e.options && e.multiple;
    const getSelectValues = options => [].reduce.call(options, (values, option) => {
      return option.selected ? values.concat(option.value) : values;
    }, []);


    let ws = new WebSocket("ws://localhost:8080/|] ++ res ++ [q|/ws");

    const keepAlive = () => {
      ws.send(JSON.stringify({}));
      setTimeout(keepAlive, 1000);
    };

    ws.onopen = e => keepAlive();
    ws.onmessage = e => document.getElementById("result").innerHTML = e.data;

    const onChangeFunc = e => {
      let element = e.target;
      let result = null;
      if (isCheckbox(element)) {
        result = element.checked;
      } else if (isMultiSelect(element)) {
        result = getSelectValues(element);
      } else if (element.type === 'range') {
        result = parseFloat(element.value);
      } else {
        result = element.value;
      }

      if (result !== null) {
        ws.send(JSON.stringify({ "element": element.id, "payload": result }));
      }
    }
  </script>
  |]
  ++
  [qc|
  <div id="result">{showMarkup a}</div>
  <table>
  |]


------------------------------------------------------------------------------
-- | The API for 'Suave' pages.
type API = Get '[HTML] Markup
      :<|> "suavemente" :> WebSocket


------------------------------------------------------------------------------
-- | The API for 'Suavely' pages.
type API2 = Capture "resource" String :> Get '[HTML] Markup
       :<|> Capture "resource" String :> "ws" :> WebSocket


------------------------------------------------------------------------------
-- | Run a 'Suave' computation by spinning up its webpage at @localhost:8080@.
suavemente :: ToMarkup a => Suave a -> IO ()
suavemente w = do
  let ws = M.singleton "" $ SomeSuave w
  run 8080
    . serve (Proxy @API)
    $ htmlHandler ws "" :<|> socketHandler ws ""


------------------------------------------------------------------------------
-- | Run a 'Suave' computation by spinning up its webpage at @localhost:8080@.
suavementely :: M.Map String SomeSuave -> IO ()
suavementely w = do
  run 8080
    . serve (Proxy @API2)
    $ htmlHandler w :<|> socketHandler w


------------------------------------------------------------------------------
-- | 'Handler' endpoint for responding to 'Suave''s websockets.
socketHandler
    :: M.Map String SomeSuave
    -> String
    -> Connection
    -> Handler ()
socketHandler ws s c =
  case M.lookup s ws of
    Nothing -> throwError err404
    Just (SomeSuave w) -> liftIO $ do
      Input _ f a <- atomically $ evalStateT (suavely w) 0
      S.effects
        . f (sendTextData c . B.pack . showMarkup =<< atomically a)
        . S.mapM (liftA2 (>>) print pure)
        . S.mapMaybe id
        . S.repeatM
        . fmap decode
        $ receiveData c


------------------------------------------------------------------------------
-- | Serve the static HTML.
htmlHandler :: M.Map String SomeSuave -> String -> Handler Markup
htmlHandler ws res =
  case M.lookup res ws of
    Nothing -> throwError err404
    Just (SomeSuave w) -> liftIO $ do
      Input html _ a <- atomically $ evalStateT (suavely w) 0
      a0 <- atomically a
      pure $ htmlPage res a0 <> html

