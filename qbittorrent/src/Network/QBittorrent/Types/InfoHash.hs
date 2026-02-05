-- | InfoHash newtype for type-safe torrent hash handling in qBittorrent API
module Network.QBittorrent.Types.InfoHash
  ( InfoHash (..)
  , hashesToText
  ) where

import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))

-- | A single qBittorrent torrent info hash (SHA-1 or SHA-256).
--
-- Use 'IsString' for convenient literal construction:
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
-- myHash :: InfoHash
-- myHash = "abc123def456..."
-- @
newtype InfoHash = InfoHash {unInfoHash :: Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (IsString, FromJSON, ToJSON, FromJSONKey, ToJSONKey, ToHttpApiData, FromHttpApiData)

-- | Join a list of info hashes into a pipe-separated 'Text' value.
--
-- >>> hashesToText [InfoHash "abc", InfoHash "def"]
-- "abc|def"
--
-- >>> hashesToText []
-- ""
hashesToText :: [InfoHash] -> Text
hashesToText = T.intercalate "|" . map (.unInfoHash)
