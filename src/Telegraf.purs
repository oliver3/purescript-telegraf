module Telegraf where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, EffFn3, EffFn4, mkEffFn1, runEffFn1, runEffFn2, runEffFn3, runEffFn4)
import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Data.Maybe (Maybe(..))

foreign import data Bot :: Type
foreign import data Context :: Type
foreign import data TELEGRAF :: Effect

type WithTelegraf eff a = ReaderT Bot (Eff eff) a
type WithContext eff a = ReaderT Context (Eff eff) a

data Configuration
  = Polling { token :: String }
  | Webhook { token :: String, url :: String, path :: String, port :: Int }

instance showConfiguration :: Show Configuration where
  show (Polling _) = "Polling"
  show (Webhook _) = "Webhook"

-- | Listen to text messages.
hears :: forall e. String -> WithContext (telegraf :: TELEGRAF | e) Unit -> WithTelegraf (telegraf :: TELEGRAF | e) Unit
hears s withContext = do
  bot <- ask
  lift $ runEffFn3 _hears bot s (mkCallback withContext)

foreign import _hears :: forall e. EffFn3 (telegraf :: TELEGRAF | e) Bot String
  (EffFn1 (telegraf :: TELEGRAF | e) Context Unit) Unit

-- | Reply with a text message.
reply :: String -> forall e. WithContext (telegraf :: TELEGRAF | e) Unit
reply msg = do
  ctx <- ask
  lift $ reply' msg ctx

reply' :: String -> Context -> forall e. Eff (telegraf :: TELEGRAF | e) Unit
reply' msg ctx = runEffFn2 _reply msg ctx

foreign import _reply :: forall e. EffFn2 (telegraf :: TELEGRAF | e) String Context Unit

type User =
  { id :: Int
  , is_bot :: Boolean
  , first_name :: String
  , last_name :: Maybe String
  , username :: Maybe String
  , language_code :: Maybe String
  }

getFrom :: forall e. WithContext (telegraf :: TELEGRAF | e) User
getFrom = do
  ctx <- ask
  lift $ runEffFn3 _getFrom Just Nothing ctx

foreign import _getFrom :: forall e. EffFn3 (telegraf :: TELEGRAF | e) (String -> Maybe String) (Maybe String) Context User

-- | Run a program within a WithTelegraf, using polling or webhook config
runWithTelegraf :: forall e. Configuration -> WithTelegraf (telegraf :: TELEGRAF | e) Unit -> Eff (telegraf :: TELEGRAF | e) Unit
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
mkCallback :: forall e. WithContext e Unit -> EffFn1 e Context Unit
mkCallback withContext = mkEffFn1 (runReaderT withContext)

