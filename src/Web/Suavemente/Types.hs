{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs         #-}

module Web.Suavemente.Types where

import           Control.Applicative (liftA2)
import           Control.Monad.STM (STM)
import           Control.Monad.State (StateT (..))
import           Data.Aeson (FromJSON (..), Value (), genericParseJSON, Options (..), defaultOptions, camelTo2)
import           GHC.Generics (Generic)
import qualified Streaming as S
import           Text.Blaze (Markup, ToMarkup (..))


------------------------------------------------------------------------------
-- | An applicative functor capable of getting input from an HTML page.
newtype Suave a = Suave
  { suavely :: StateT Int STM (Input a)
  } deriving Functor

instance Applicative Suave where
  pure = Suave . pure . pure
  Suave f <*> Suave a = Suave $ liftA2 (<*>) f a


------------------------------------------------------------------------------
-- | An existentialized 'Suave'.
data SomeSuave where
  SomeSuave :: (a -> Markup) -> Suave a -> SomeSuave


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
           -> S.Stream (S.Of ChangeEvent) IO ()
           -> S.Stream (S.Of ChangeEvent) IO ()

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
-- | Change messages that come from the JS side.
data ChangeEvent = ChangeEvent
  { ceElement :: String
  , cePayload :: Value
  } deriving (Eq, Show, Generic)

instance FromJSON ChangeEvent where
  parseJSON = genericParseJSON $ defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 2
    }

