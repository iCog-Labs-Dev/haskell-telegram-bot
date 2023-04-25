{-# LANGUAGE DeriveGeneric #-}

module Update where

import Data.Aeson
import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)
import TelegramTypes

-- | represents an incoming update. At most one of the optional parameters can be present
-- in any given update.
data Update = Update
  { update_id :: Int, -- The update's unique identifier
    message :: Maybe Message, -- New incoming message of any kind - text, photo, sticker, etc.
    edited_message :: Maybe Message, -- New version of a message that is known to the bot and was edited
    channel_post :: Maybe Message, -- New incoming channel post of any kind - text, photo, sticker, etc.
    edited_channel_post :: Maybe Message, -- New version of a channel post that is known to the bot and was edited
    inline_query :: Maybe InlineQuery, -- New incoming inline query
    chosen_inline_result :: Maybe ChosenInlineResult, -- The result of an inline query that was chosen by a user and sent to their chat partner
    callback_query :: Maybe CallbackQuery, -- New incoming callback query
    shipping_query :: Maybe ShippingQuery, -- New incoming shipping query. Only for invoices with flexible price
    pre_checkout_query :: Maybe PreCheckoutQuery, -- New incoming pre-checkout query. Contains full information about checkout
    poll :: Maybe Poll, -- New poll state. Bots receive only updates about stopped polls and polls, which are sent by the bot
    poll_answer :: Maybe PollAnswer, -- A user changed their answer in a non-anonymous poll. Bots receive new votes only in polls that were sent by the bot itself.
    my_chat_member :: Maybe ChatMemberUpdated, -- The bot's chat member status was updated in a chat. For private chats, this update is received only when the bot is blocked or unblocked by the user.
    chat_member :: Maybe ChatMemberUpdated, -- A chat member's status was updated in a chat. The bot must be an administrator in the chat and must explicitly specify “chat_member” in the list of allowed_updates to receive these updates.
    chat_join_request :: Maybe ChatJoinRequest -- A request to join the chat has been sent. The bot must have the can_invite_users administrator right in the chat to receive these updates.
  }
  deriving (Show, Eq, Generic)

makeUpdate :: Aeson.Value -> Maybe Update
makeUpdate = decode . encode

instance FromJSON Update

instance ToJSON Update
