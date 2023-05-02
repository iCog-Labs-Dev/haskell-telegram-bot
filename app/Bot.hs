{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use map" #-}

-- |
-- Module      : Bot
-- Description : contains representations and functions related to a telegram Bot.
-- Copyright   : (c) Surafel firku, 2023
-- License     : MIT
-- Maintainer  : surafelfikru76@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Here is a longer description of this module, containing some
-- commentary with @some markup@.
module Bot
  ( Bot (..),
    BotBuilder (..),
    initBot,
    withToken,
    argValidator,
    getUpdates,
    buildBot,
    BotError (..),
  )
where

-- import qualified Data.ByteString.Lazy.Char8 as L8

-- import qualified Network.URI as URL

import Control.Lens
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as AesonKm
import qualified Data.Map as Map
import qualified Data.Maybe as Mb
import qualified Data.Maybe as Mb.Maybe
import qualified Data.Text as T
import qualified Data.Vector as Vector
import qualified Network.HTTP.Simple as Http
import TelegramTypes
import Update

-- | a type representing a telegram Bot. it holds data such as:
--    * @telegramHost@ - the base url for the telegram host
--    * @tokenPrefix@ - a prefix of the token used prior to a token. telegram requires
--    the token to be prefixed by 'bot' in the URL. this is set as 'bot' by the constructor.
--    * @token@ - a telegram bot token obtained from the bot father
-- Note that you should not access this type directly but use the bot functino which uses
-- the BotConfig type.
data Bot = Bot
  { botHost :: String,
    botTokenPrefix :: String,
    botToken :: String
  }
  deriving (Show, Eq)

makeLenses ''Bot

-- | a public type that mimics the Bot type without including private fields. you can use
-- this type to construct a bot and pass it to the bot function.
--    * @telegramHost@ - the base url for the telegram host
--    * @token@ - a telegram bot token obtained from the bot father
newtype BotBuilder = BotBuilder
  { builderToken :: String
  }
  deriving (Show, Eq)

-- | a type that represents and encapsulates all the arguments a function that is applied
-- on a Bot can take. A value usually will not be an Object
type Args = Map.Map String Aeson.Value

type Validator = Map.Map String (Bool, Aeson.Value)

type Token = String

data BotError
  = ArgumentRequiredError String
  | ArgumentTypeError String
  | HttpError String
  | TelegramError String
  deriving (Show, Eq)

-- Functions

-- | a BotBuilder constructor. returns the minimal form of BotBuilder
initBot :: BotBuilder
initBot = BotBuilder ""

-- | a builder function for the BotBuilder type that adds a token
withToken :: String -> BotBuilder -> BotBuilder
withToken token builder = builder {builderToken = token}

-- | the final call on a BotBuilder that returns a Bot
buildBot :: BotBuilder -> Bot
buildBot builder =
  Bot
    { botHost = "https://api.telegram.org",
      botTokenPrefix = "bot",
      botToken = builderToken builder
    }

-- | Generate a bot from token
getBot :: Token -> Bot
getBot token = buildBot $ withToken token initBot

telegramEndpoint :: Bot -> String
telegramEndpoint bt = botHost bt ++ "/" ++ botTokenPrefix bt ++ botToken bt

appendMethod :: Bot -> String -> String
appendMethod bt method = telegramEndpoint bt ++ "/" ++ method

getMethod :: Aeson.FromJSON j => Bot -> String -> IO (Either String j)
getMethod bt method =
  let uri = appendMethod bt method
   in do
        req <- Http.parseRequest uri
        res <- Http.httpJSONEither req
        case Http.getResponseBody res of
          Left err -> return (Left ("error request for method" ++ method ++ ":" ++ show err))
          Right ok -> return (Right ok)

postMethod :: Aeson.ToJSON j => Bot -> String -> j -> IO (Either String Aeson.Value)
postMethod bt method args =
  let uri = appendMethod bt method
      buildRequest :: Aeson.ToJSON a => a -> Http.Request -> Http.Request
      buildRequest args =
        Http.setRequestHeader "Content-Type" ["application/json"]
          . Http.setRequestMethod "POST"
          . Http.setRequestBodyJSON args
   in do
        req <- Http.parseRequest uri
        res <- Http.httpJSONEither (buildRequest args req)
        case Http.getResponseBody res of
          Left err -> return (Left ("error request for method" ++ method ++ ": " ++ show err))
          Right ok -> return (Right ok)

postMethodWithFile :: Aeson.ToJSON j => Bot -> String -> Aeson.Value -> IO (Either String j)
postMethodWithFile = undefined

getKey :: Aeson.Value -> AesonKm.Key -> Maybe Aeson.Value
getKey val k = case val of
  Aeson.Object o -> AesonKm.lookup k o

checkOkKey :: Aeson.Value -> Bool
checkOkKey val = getKey val "ok" == Just (Aeson.Bool True)

parseResult :: Aeson.FromJSON a => Either String Aeson.Value -> IO (Either BotError [Maybe a])
parseResult a = case a of
  Left err -> return $ Left (HttpError (show err))
  Right ok ->
    return $
      if checkOkKey ok
        then
          Right
            ( case getKey ok "result" of
                Just (Aeson.Array a) -> foldl (\acc a -> (Aeson.decode . Aeson.encode) a : acc) [] a
            )
        else
          Left
            ( -- ISSUE: the display errors where the ok key is false could be better and more native
              TelegramError ("there is an issue with the telegram response object\nResponse:\n" ++ show ok)
            )

-- | validates if a given argument is valid for a certain schema. this is used
--  by telegram bot api method functions to ensure required arguments are present and
--  check if all of the fields have valid types
-- Note:
--   * the validator does not care for additional keys put in args
argValidator ::
  -- | arguments that are going to be checked
  Map.Map String Aeson.Value ->
  -- | schema used to validate the arguments
  Validator ->
  -- | a safe Either type to tell if something is valid or not
  Either BotError Bool
argValidator args = Map.foldlWithKey validate (Right True)
  where
    validate :: Either BotError Bool -> String -> (Bool, Aeson.Value) -> Either BotError Bool
    validate acc k v =
      case acc of
        Left (ArgumentRequiredError err) -> Left (ArgumentRequiredError err)
        Left (ArgumentTypeError err) -> Left (ArgumentTypeError err)
        _ ->
          case Map.lookup k args of
            Nothing -> if fst v then Left (ArgumentRequiredError (k ++ " is a required argument but not provided")) else acc
            Just val -> if getType val /= getType (snd v) then Left (ArgumentTypeError ("argument should be of type '" ++ getType (snd v) ++ "' but it is set as '" ++ getType val)) else acc
              where
                getType :: Aeson.Value -> String
                getType val = head $ words $ show val

callMethod :: Aeson.FromJSON j => String -> Validator -> Bot -> Args -> IO (Either BotError [Maybe j])
callMethod method validator bt args =
  case isValid of
    Left err -> return $ Left err
    Right True -> postMethod bt method args >>= parseResult
  where
    isValid = argValidator args validator

-- | a functions used to receive incoming updates using long polling. Returns an Array of
-- Update objects. accepts parameters as listed below.
--    @param offset An optional field that is an identifier of the first update to be returned.
--    Must be greater by one than the highest among the identifiers of previously received updates.
--    By default, updates starting with the earliest unconfirmed update are returned. it is possible
--    to use negative offsets.
--
--    @param limit An optional field that limits the number of updates to be retrieved. Values
--    between 1-100 are accepted. Defaults to 100.
--
--    @param timeout An optional integer field that represents timeout in seconds for long polling.
--
--    @param allowed_updates An optional list of the update types you want your bot to receive.
--
-- Note that ths method will not work if a webhook is setup.
getUpdates :: Bot -> Args -> IO (Either BotError [Maybe Update])
getUpdates =
  callMethod
    "getUpdates"
    ( Map.fromList
        [ ("offset", (False, Aeson.String "")),
          ("limit", (False, Aeson.Number 0)),
          ("timeout", (False, Aeson.Number 0)),
          ("allowed_updates", (False, Aeson.Array Vector.empty))
        ]
    )

-- | A simple method for testing your bot's authentication token. Requires no parameters.
-- Returns basic information about the bot in form of a User object.
getMe :: Bot -> IO (Either BotError [Maybe User])
getMe bt =
  callMethod "getMe" Map.empty bt Map.empty

-- let method = "getMe"
--  in postMethod bt method Aeson.Null
--       >>= parseResult

-- | a function used to to specify a URL and receive incoming updates via an outgoing webhook.
-- accepts parameters as listed below. Whenever there is an update for the bot, telegram will
-- send an HTTPS POST request to the specified URL, containing a JSON-serialized Update
--    @param url A required HTTPS URL to send updates to.
--
--    @param certificate An optional public key certificate so that the root certificate in use can be checked,
--
--    @param ip_address An optional fixed IP address which will be used to send webhook requests instead
--    of the IP address resolved through DNS.
--
--    @param max_connections An optional integer field the represents the maximum allowed number of
--    simultaneous HTTPS connections to the webhook for update delivery, accepts 1-100 and defaults to 40.
--
--    @param allowed_updates An optional list of the update types you want your bot to receive.
--
--    @param drop_pending_updates A boolean that if set to True drops updates for your bot that have not yet
--    been delivered, and your bot will only receive updates that arrive after the webhook is set.
--
--    @param secret_token Allows you to add an additional layer of security to your webhook by verifying
--    that the incoming requests are coming from Telegram.
setWebhook :: Bot -> Args -> IO (Either BotError [Maybe Aeson.Value])
setWebhook =
  callMethod
    "setWebhook"
    ( Map.fromList
        [ ("url", (True, Aeson.String "")),
          ("certificate", (False, Aeson.Null)), -- ISSUE: does not valiadte against telegram types. In this case InputFile
          ("ip_address", (False, Aeson.String "")),
          ("max_connections", (False, Aeson.Number 0)),
          ("allowed_updates", (False, Aeson.Array (Vector.singleton ""))), -- ISSUE: does not validate nested datastructres. In this case (Aeson.Array String)
          ("drop_pending_updates", (False, Aeson.Bool True)),
          ("secret_token", (False, Aeson.String ""))
        ]
    )

-- | removes webhook integration. accepts args that has parameters depicted below.
--    @param drop_pending_updates drops all pending updates.
deleteWebhook :: Bot -> Args -> IO (Either BotError [Maybe Aeson.Value])
deleteWebhook =
  callMethod
    "deleteWebhook"
    ( Map.fromList
        [ ("drop_pending_updates", (True, Aeson.Bool True))
        ]
    )

-- | Used to get current webhook status. accepts no parameters.
getWebhookInfo :: Bot -> IO (Either BotError [Maybe WebhookInfo])
getWebhookInfo bt =
  callMethod "getWebhookInfo" Map.empty bt Map.empty

-- | Send a simple text message using chat id and message text.ccepts args that has parameters depicted below.
--    @param chat_id Unique identifier for the target chat or username of the target channel.
--    @param message_thread_id Unique identifier for the target message thread (topic)
--    @param text Text of the message to be sent
--    @param parse_mode Mode for parsing entities in the message text. 
--    @param entities A JSON-serialized list of special entities that appear in message text
--    @param disable_web_page_preview Disables link previews for links in this message.
--    @param disable_notification Sends the message silently.
--    @param protect_content Protects the contents of the sent message from forwarding and saving.
--    @param reply_to_message_id If the message is a reply, ID of the original message.
--    @param allow_sending_without_reply Pass True if the message should be sent even if
--    the specified replied-to message is not found.
--    @param reply_markup Pass Additional interface options. A JSON-serialized object for
--    an inline keyboard, custom reply keyboard, instructions to remove reply keyboard 
--    or to force a reply from the user.

sendMessage :: Bot -> Args -> IO (Either BotError [Maybe Aeson.Value])
sendMessage =
  callMethod
    "sendMessage"
    ( Map.fromList
        [ ("chat_id", (True, Aeson.String "")),
          ("message_thread_id", (False, Aeson.Number 0)),
          ("text", (True, Aeson.String "")),
          ("parse_mode", (False, Aeson.String "")),
          ("entities", (False, Aeson.Array Vector.empty))
          ("disable_web_page_preview", (False, Aeson.Bool False)),
          ("disable_notification", (False, Aeson.Bool False)),
          ("protect_content", (False, Aeson.Bool False)),
          ("reply_to_message_id", (False, Aeson.Number 0)),
          ("allow_sending_without_reply", (False, Aeson.Bool False)),
          ("reply_markup", (True, Aeson.Null)) -- ISSUE: does not valiadte against telegram types. In this case InlineKeyBoardMarkup, ReplyKeyboardMarkup, ReplyKeyboardRemove or ForceReply.
        ]
    )