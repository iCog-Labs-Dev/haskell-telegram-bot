# haskell-telegram-bot

[![docs](https://img.shields.io/badge/docs-repo-blue.svg)](https://github.com/arist76/haskell-telegram-bot)
[![telegram bot api](https://img.shields.io/badge/telegram%20bot%20api-https%3A%2F%2Fcore.telegram.org%2Fbots%2Fapi-blue.svg)](https://core.telegram.org/bots/api)
[![python-telegram-bot api](https://img.shields.io/badge/python--telegram--bot%20api-https%3A%2F%2Fdocs.python--telegram--bot.org%2Fen%2Fstable%2F-blue.svg)](https://docs.python-telegram-bot.org/en/stable/)
[![coverage](https://img.shields.io/badge/coverage-100%25-brightgreen.svg)](#)
[![license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![ghc version](https://img.shields.io/badge/ghc%20version-9.2.5-blue.svg)](#)
[![issue tracker](https://img.shields.io/badge/issue%20tracker-repo-blue.svg)](https://github.com/arist76/haskell-telegram-bot/issues)

A simple and pure API implementation for the Telegram Bot API in Haskell. It aims to provide a type-safe way to build Telegram bots using Haskell's strong type system, making it easy to create maintainable and robust bots.

## Features
- Simple to use
- Helps to build a type-safe Telegram bot
- Utilizes Haskell's type safety to create awesome, maintainable, and easy-to-implement bots

## Installation
<!-- Not published yet. coming soon -->

## Usage
This repo Basically defines a Bot data type that represents a Telegram bot and provides functions for constructing and interacting with the Telegram bot API. The module includes functions for making HTTP requests to the Telegram API which take Bot as an argument.

The Bot can be accessed throught the BotBuilder data type which is an implementation of the builder design pattern. BotBuilder is a minimal representation of the Bot type without including private fields. It provides a constructor initBot for creating a BotBuilder with an empty token, and a withToken function for setting the token on a BotBuilder instance. The buildBot function is used to convert a BotBuilder instance to a Bot instance.

```Haskell
import Bot

builder :: BotBuilder
builder = initBot

-- Add the token to the builder
builderWithToken :: BotBuilder
builderWithToken = 
    withToken
        "123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11"
        builder

-- Build the Bot instance
myBot :: Bot
myBot = buildBot builder
```

All the Telegram bot methods are (will be) implemented. so you can use each method like this
```Haskell
updates :: IO (Either BotError [Update])
updates = getUpdates
```
is similar to 
``` curl
curl https://api.telegram.org/bot123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11/getUpdates
```

## Documentation
<!-- Not published yet. coming soon>

## Contributing
<!-- contribution guidelines coming soon here -->

## License
This project is released under the [MIT License](LICENSE).

## GHC Version
This library is currently compatible with GHC version 9 and above

## Issue Tracker
Please report any bugs or issues in the [Issue Tracker](https://github.com/arist76/haskell-telegram-bot/issues).

