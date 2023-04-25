{-# LANGUAGE DeriveGeneric #-}

module TelegramTypes where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)

type Message = Aeson.Value

type User = Aeson.Value

type Chat = Aeson.Value

type InlineQuery = Aeson.Value

type ChosenInlineResult = Aeson.Value

type CallbackQuery = Aeson.Value

type ShippingQuery = Aeson.Value

type PreCheckoutQuery = Aeson.Value

type Poll = Aeson.Value

type PollAnswer = Aeson.Value

type ChatMemberUpdated = Aeson.Value

type ChatJoinRequest = Aeson.Value

type WebhookInfo = Aeson.Value

-- instance FromJSON Message

-- instance ToJSON Message

-- instance FromJSON User

-- instance ToJSON User

-- instance FromJSON Chat

-- instance ToJSON Chat