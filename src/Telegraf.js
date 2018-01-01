const Telegraf = require('telegraf')

exports.constructImpl = function (token) {
  const bot = new Telegraf(token, {})
  return bot
}

exports.startPollingImpl = function (bot) {
  bot.telegram.deleteWebhook()
  bot.startPolling()
}

exports.startWebhookImpl = function (bot, url, path, port) {
  bot.telegram.setWebhook(url)
  bot.startWebhook(path, null, port)
}

exports.hearsImpl = function (bot, s, cb) {
  bot.hears(s, cb)
}

exports.replyImpl = function (s, ctx) {
  ctx.reply(s)
}

exports.getFromImpl = function (just, nothing, ctx) {
  return {
    id: Number(ctx.from.id) | 0,
    is_bot: Boolean(ctx.from.is_bot),
    first_name: String(ctx.from.first_name),
    last_name: ctx.from.last_name ? just(String(ctx.from.last_name)) : nothing,
    username: ctx.from.username ? just(String(ctx.from.username)) : nothing,
    language_code: ctx.from.language_code ? just(String(ctx.from.language_code)) : nothing
  }
}

exports.getChatImpl = function (just, nothing, ctx) {
  return {
    id: Number(ctx.chat.id) | 0,
    type: String(ctx.chat.type), // private, group, supergroup, channel
    title: ctx.chat.title ? just(String(ctx.chat.title)) : nothing,
    username: ctx.chat.username ? just(String(ctx.chat.username)) : nothing,
    first_name: ctx.chat.first_name ? just(String(ctx.chat.first_name)) : nothing,
    last_name: ctx.chat.last_name ? just(String(ctx.chat.last_name)) : nothing,
  }
}

exports.sendMessageImpl = function (bot, id, msg) {
  bot.telegram.sendMessage(id, msg)
}