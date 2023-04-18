{-# LANGUAGE DeriveGeneric #-}

module Update where

import Data.Aeson
import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)
import TelegramTypes

-- | represents an incoming update. At most one of the optional parameters can be present
-- in any given update.
data Update = Update
  { -- | Unique identifier for the update
    updateId :: Int,
    -- | Optional. New incoming message
    message :: Maybe Message
  }
  deriving (Show, Generic)

makeUpdate :: Aeson.Value -> Maybe Update
makeUpdate = decode . encode

instance FromJSON Update

instance ToJSON Update
