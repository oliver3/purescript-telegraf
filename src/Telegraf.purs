module Telegraf where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, EffFn3, mkEffFn1, runEffFn1, runEffFn2, runEffFn3)
import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)

foreign import data Bot :: Type
foreign import data Context :: Type
foreign import data TELEGRAF :: Effect

type WithTelegraf a = forall e. ReaderT Bot (Eff (telegraf :: TELEGRAF | e)) a
type WithContext a = forall e. ReaderT Context (Eff (telegraf :: TELEGRAF | e)) a

-- | Initialize new Telegraf bot.
construct :: forall e. String -> Eff (telegraf :: TELEGRAF | e) Bot
construct = runEffFn1 _construct
foreign import _construct :: forall e. EffFn1 (telegraf :: TELEGRAF | e) String Bot

-- | Start poll updates.
startPolling :: forall e. Bot -> Eff (telegraf :: TELEGRAF | e) Unit
startPolling = runEffFn1 _startPolling
foreign import _startPolling :: forall e. EffFn1 (telegraf :: TELEGRAF | e) Bot Unit

-- | Listen to text messages.
hears :: forall e. String -> WithContext Unit -> WithTelegraf Unit
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

-- | Run a program within a WithTelegraf using polling and the token supplied
runWithTelegraf :: forall e. String -> WithTelegraf Unit -> Eff (telegraf :: TELEGRAF | e) Unit
runWithTelegraf token withTelegraf = do
  bot <- construct token
  runReaderT withTelegraf bot
  startPolling bot

-- | Make a FFI callback function from the WithContext
mkCallback :: forall e ctx. ReaderT ctx (Eff e) Unit -> EffFn1 e ctx Unit
mkCallback withContext = mkEffFn1 (runReaderT withContext)

