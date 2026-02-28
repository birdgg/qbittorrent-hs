module Network.QBittorrent.Types.Tag
  ( Tag (..)
  , tagsToText
  , textToTags
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

-- | A single qBittorrent tag name.
--
-- Use 'IsString' for convenient literal construction:
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
-- myTag :: Tag
-- myTag = "linux-iso"
-- @
newtype Tag = Tag {unTag :: Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (IsString, FromJSON, ToJSON)

-- | Join a list of tags into a comma-separated 'Text' value.
--
-- >>> tagsToText [Tag "foo", Tag "bar"]
-- "foo,bar"
--
-- >>> tagsToText []
-- ""
tagsToText :: [Tag] -> Text
tagsToText = T.intercalate "," . map (.unTag)

-- | Split a comma-separated 'Text' value into a list of tags.
-- Empty segments and whitespace-only segments are dropped.
--
-- >>> textToTags "foo, bar , baz"
-- [Tag "foo",Tag "bar",Tag "baz"]
--
-- >>> textToTags ""
-- []
textToTags :: Text -> [Tag]
textToTags t
  | T.null (T.strip t) = []
  | otherwise = map (Tag . T.strip) $ T.splitOn "," t
