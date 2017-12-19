
PureScript interface for the Telegraf [1] bot framework, using ReaderT to avoid having to supply the `bot` or `ctx` argument every time


[1] http://telegraf.js.org/

```sh
bower install purescript-telegraf --save
npm install telegraf --save
```


```PureScript
module Main where

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

```