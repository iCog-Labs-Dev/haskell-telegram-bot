{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

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
    BotError (..),
  )
where

-- import qualified Data.ByteString.Lazy.Char8 as L8

-- import qualified Network.URI as URL

import Control.Lens
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Vector as Vector
import qualified Network.HTTP.Simple as Http
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

-- | validates if a given argument is valid for a certain schema. this is used
--  by telegram bot api method functions to ensure required arguments are present and
--  check if all of the fields have valid types
-- Note:
--   * the validator does not care for additional keys put in args
argValidator ::
  -- | arguments that are going to be checked
  Map.Map String Aeson.Value ->
  -- | schema used to validate the arguments
  Map.Map String (Bool, Aeson.Value) ->
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

-- | a functions used to receive incoming updates using long polling. Returns an Array of
-- Update objects. gets its arguments through the Args type which are documented below
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
getUpdates :: Bot -> Map.Map String Aeson.Value -> IO (Either BotError [Update])
getUpdates bt args =
  case isValid of
    Left err -> return $ Left err
    Right True ->
      let method = "getUpdates"
       in postMethod bt method args
            >>= ( \a -> case a of
                    Left err -> return $ Left (HttpError (show err))
                    Right ok ->
                      return $
                        Right
                          ( case ok of
                              Aeson.Array a -> Vector.toList (Vector.map (fromMaybe (error "an invalid Update") . makeUpdate) a)
                              _ -> error "An array should not come here"
                          )
                )
  where
    isValid =
      argValidator
        args
        ( Map.fromList
            [ ("offset", (False, Aeson.String "")),
              ("limit", (False, Aeson.Number 0)),
              ("timeout", (False, Aeson.Number 0)),
              ("allowed_updates", (False, Aeson.Array Vector.empty))
            ]
        )

-- | a function used to to specify a URL and receive incoming updates via an outgoing webhook.
-- accepts parameters in the form of Args. the parameters are listed below.
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
setWebhook :: Bot -> Args -> IO Bool
setWebhook = undefined

-- | removes webhook integration. accepts args that has parameters depicted below.
--    @param drop_pending_updates drops all pending updates.
deleteWebhook :: Bot -> Args -> IO Bool
deleteWebhook = undefined

-- | Used to get current webhook status. accepts no parameters.
getWebhookInfo :: Bot -> WebhookInfo
getWebhookInfo = undefined

----------------
-- Type synonyms
----------------

-- | a type that represents and encapsulates all the arguments a function that is applied
-- on a Bot can take. A value usually will not be an Object
type Args = Map.Map String Aeson.Value

------------------------------
-- Algebraic type declarations
------------------------------

-- | Describes the current status of a webhook.
data WebhookInfo = WebhookInfo

data BotError
  = ArgumentRequiredError String
  | ArgumentTypeError String
  | HttpError String
  deriving (Show, Eq)