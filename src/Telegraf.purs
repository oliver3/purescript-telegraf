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
hears s respond = do
  bot <- ask
  lift $ runEffFn3 hearsImpl bot s (mkCallback respond)

foreign import hearsImpl :: forall e. EffFn3 (telegraf :: TELEGRAF | e) Bot String
  (EffFn1 (telegraf :: TELEGRAF | e) Context Unit) Unit

-- | Reply with a text message.
reply :: String -> forall e. WithContext (telegraf :: TELEGRAF | e) Unit
reply msg = withContext $ reply' msg

reply' :: String -> Context -> forall e. Eff (telegraf :: TELEGRAF | e) Unit
reply' msg ctx = runEffFn2 replyImpl msg ctx

foreign import replyImpl :: forall e. EffFn2 (telegraf :: TELEGRAF | e) String Context Unit

type User =
  { id :: Int
  , is_bot :: Boolean
  , first_name :: String
  , last_name :: Maybe String
  , username :: Maybe String
  , language_code :: Maybe String
  }

type Chat =
  { id :: Int
  , type :: String -- private, group, supergroup, channel
  , title :: Maybe String
  , username :: Maybe String
  , first_name :: Maybe String
  , last_name :: Maybe String
  -- , all_members_are_administrators :: Maybe Boolean
  }

getFrom :: forall e. WithContext (telegraf :: TELEGRAF | e) User
getFrom = withContext $ runEffFn3 getFromImpl Just Nothing

foreign import getFromImpl :: forall e. EffFn3 (telegraf :: TELEGRAF | e) (String -> Maybe String) (Maybe String) Context User

getChat :: forall e. WithContext (telegraf :: TELEGRAF | e) Chat
getChat = withContext $ runEffFn3 getChatImpl Just Nothing

foreign import getChatImpl :: forall e. EffFn3 (telegraf :: TELEGRAF | e) (String -> Maybe String) (Maybe String) Context Chat

sendMessage :: forall e. Int -> String -> WithTelegraf (telegraf :: TELEGRAF | e) Unit
sendMessage id msg = do
  bot <- ask
  lift $ runEffFn3 sendMessageImpl bot id msg

foreign import sendMessageImpl :: forall e. EffFn3 (telegraf :: TELEGRAF | e) Bot Int String Unit

-- | Run a program within a WithTelegraf, using polling or webhook config
runWithTelegraf :: forall e. Configuration -> WithTelegraf (telegraf :: TELEGRAF | e) Unit -> Eff (telegraf :: TELEGRAF | e) Unit
runWithTelegraf (Polling {token}) withTelegraf = do
  bot <- runEffFn1 constructImpl token
  runReaderT withTelegraf bot
  runEffFn1 startPollingImpl bot
runWithTelegraf (Webhook {token, url, path, port}) withTelegraf = do
  bot <- runEffFn1 constructImpl token
  runReaderT withTelegraf bot
  runEffFn4 startWebhookImpl bot url path port

foreign import constructImpl :: forall e. EffFn1 (telegraf :: TELEGRAF | e) String Bot
foreign import startPollingImpl :: forall e. EffFn1 (telegraf :: TELEGRAF | e) Bot Unit
foreign import startWebhookImpl :: forall e. EffFn4 (telegraf :: TELEGRAF | e) Bot String String Int Unit

-- | Make a FFI callback function from the WithContext
mkCallback :: forall e. WithContext e Unit -> EffFn1 e Context Unit
mkCallback withContext = mkEffFn1 (runReaderT withContext)

-- | Wrap a function that needs a Context in the WithContext monad
withContext :: forall eff a. (Context -> Eff eff a) -> WithContext eff a
withContext fn = do
  ctx <- ask
  lift $ fn ctx
