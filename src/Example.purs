module Example where

import Prelude

import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff (Eff)
import Telegraf (Configuration(..), TELEGRAF, getFrom, hears, reply, runWithTelegraf)

config :: Configuration
config = Polling { token: "My Telegram token from BotFather" }

main :: forall eff. Eff (telegraf :: TELEGRAF | eff) Unit
main = launchAff_ $ runWithTelegraf config do
  hears "hi" do
    user <- getFrom
    reply $ "Hey " <> user.first_name
    reply "What's up?"