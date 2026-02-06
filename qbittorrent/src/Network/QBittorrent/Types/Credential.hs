-- | Credential type for type-safe credential handling in qBittorrent API
module Network.QBittorrent.Types.Credential
  ( Credential (..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)

-- | qBittorrent authentication credentials
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
-- myCred :: Credential
-- myCred = Credential { username = "admin", password = "secret" }
-- @
data Credential = Credential
  { username :: Text
  , password :: Text
  }
  deriving stock (Show, Eq, Generic)
