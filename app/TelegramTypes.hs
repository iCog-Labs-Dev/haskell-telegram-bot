{-# LANGUAGE DeriveGeneric #-}

module TelegramTypes where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data Message = Message
  { -- | Unique identifier for the message.
    messageId :: Integer,
    -- | Optional. Unique identifier of a message thread to which the message belongs;
    -- for supergroups only.
    messageThreadId :: Maybe Integer,
    -- | Sender of the message.
    from :: User,
    -- | Chat the message belongs to.
    chat :: Chat,
    -- | Optional. Sender of the message, sent on behalf of a chat.
    senderChat :: Maybe Chat,
    -- | Date the message was sent in Unix time.
    date :: Integer,
    -- | Optional. Used for forwarded messages, contains sender of the original message.
    forwardFrom :: Maybe User,
    -- | Optional. For messages forwarded from channels or from anonymous administrators,
    -- contains information about the original sender chat.
    forwardFromChat :: Maybe Chat,
    -- | Optional. For messages forwarded from channels, contains identifier of the original
    -- message in the channel.
    forwardFromMessageId :: Maybe Integer,
    -- | Optional. For forwarded messages that were originally sent in channels or by an
    -- anonymous chat administrator, contains signature of the message sender if presen
    forwardSignature :: Maybe String,
    -- | Optional. Sender's name for messages forwarded from users who disallow
    -- adding a link to their account in forwarded messages
    forwardSenderName :: Maybe String,
    -- | Optional. For forwarded messages, date the original message was sent in Unix time
    forwardDate :: Maybe Integer,
    -- | Optional. True, if the message is sent to a forum topic
    isTopicMessage :: Maybe Bool,
    -- | Optional. True, if the message is a channel post that was automatically
    -- forwarded to the connected discussion group otherwise it will not be specified
    isAutomaticForward :: Maybe Bool,
    -- | Optional. For replies, the original message. Note that the Message object
    -- in this field will not contain further reply_to_message fields even if it itself is a reply.
    replyToMessage :: Maybe Message,
    -- | Optional. Bot through which the message was sent
    viaBot :: Maybe User,
    -- | Optional. Date the message was last edited in Unix time
    editDate :: Maybe Integer,
    -- | Optional. True, if the message can't be forwarded
    hasProtectedContent :: Maybe Bool,
    -- | Optional. Text of the message
    text :: Maybe String
  }
  deriving (Show, Generic)

data User = User deriving (Show, Generic)

data Chat = Chat deriving (Show, Generic)

instance FromJSON Message

instance ToJSON Message

instance FromJSON User

instance ToJSON User

instance FromJSON Chat

instance ToJSON Chat