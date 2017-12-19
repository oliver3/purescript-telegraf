const Telegraf = require('telegraf')

exports._construct = function (token) {
  const bot = new Telegraf(token, {})
  return bot
}

exports._startPolling = function (bot) {
  bot.startPolling()
}

exports._hears = function (bot, s, cb) {
  bot.hears(s, cb)
}

exports._reply = function (s, ctx) {
  ctx.reply(s)
}
