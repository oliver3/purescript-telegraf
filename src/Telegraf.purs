module Telegraf where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, EffFn3, EffFn4, mkEffFn1, runEffFn1, runEffFn2, runEffFn3, runEffFn4)
import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Data.Maybe (Maybe(..))

foreign import data Bot :: Type
foreign import data Context :: Type
foreign import data TELEGRAF :: Effect

type WithTelegraf a = forall e. ReaderT Bot (Eff (telegraf :: TELEGRAF | e)) a
type WithContext a = forall e. ReaderT Context (Eff (telegraf :: TELEGRAF | e)) a

data Configuration
  = Polling { token :: String }
  | Webhook { token :: String, url :: String, path :: String, port :: Int }

instance showConfiguration :: Show Configuration where
  show (Polling _) = "Polling"
  show (Webhook _) = "Webhook"

-- | Listen to text messages.
hears :: String -> WithContext Unit -> WithTelegraf Unit
hears s withContext = do
  bot <- ask
  lift $ runEffFn3 _hears bot s (mkCallback withContext)

foreign import _hears :: forall e. EffFn3 (telegraf :: TELEGRAF | e) Bot String
  (EffFn1 (telegraf :: TELEGRAF | e) Context Unit) Unit

-- | Reply with a text message.
reply :: String -> WithContext Unit
reply s = do
  ctx <- ask
  lift $ runEffFn2 _reply s ctx

foreign import _reply :: forall e. EffFn2 (telegraf :: TELEGRAF | e) String Context Unit

type User =
  { id :: Int
  , is_bot :: Boolean
  , first_name :: String
  , last_name :: Maybe String
  , username :: Maybe String
  , language_code :: Maybe String
  }

getFrom :: WithContext User
getFrom = do
  ctx <- ask
  lift $ runEffFn3 _getFrom Just Nothing ctx

foreign import _getFrom :: forall e. EffFn3 (telegraf :: TELEGRAF | e) (String -> Maybe String) (Maybe String) Context User

-- | Run a program within a WithTelegraf, using polling or webhook config
runWithTelegraf :: forall e. Configuration -> WithTelegraf Unit -> Eff (telegraf :: TELEGRAF | e) Unit
runWithTelegraf (Polling {token}) withTelegraf = do
  bot <- runEffFn1 _construct token
  runReaderT withTelegraf bot
  runEffFn1 _startPolling bot
runWithTelegraf (Webhook {token, url, path, port}) withTelegraf = do
  bot <- runEffFn1 _construct token
  runReaderT withTelegraf bot
  runEffFn4 _startWebhook bot url path port

foreign import _construct :: forall e. EffFn1 (telegraf :: TELEGRAF | e) String Bot
foreign import _startPolling :: forall e. EffFn1 (telegraf :: TELEGRAF | e) Bot Unit
foreign import _startWebhook :: forall e. EffFn4 (telegraf :: TELEGRAF | e) Bot String String Int Unit

-- | Make a FFI callback function from the WithContext
mkCallback :: forall e ctx. ReaderT ctx (Eff e) Unit -> EffFn1 e ctx Unit
mkCallback withContext = mkEffFn1 (runReaderT withContext)

