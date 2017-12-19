module Example where

import Prelude

import Control.Monad.Eff (Eff)
import Telegraf (TELEGRAF, Configuration(..), hears, reply, runWithTelegraf)

config :: Configuration
config = Polling { token: "My Telegram token from BotFather" }

main :: forall eff. Eff (telegraf :: TELEGRAF | eff) Unit
main = runWithTelegraf config do
  hears "hi" do
    reply "Hey there!"
    reply "What's up?"