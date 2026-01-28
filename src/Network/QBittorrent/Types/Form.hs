-- | Form types for qBittorrent API (servant-client)
module Network.QBittorrent.Types.Form
  ( LoginForm (..)
  , HashesForm (..)
  , DeleteTorrentsForm (..)
  , TagsForm (..)
  , RenameFileForm (..)
  , RenameFolderForm (..)
  , SetLocationForm (..)
  , PreferencesForm (..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Web.FormUrlEncoded (ToForm)

-- | Login form
data LoginForm = LoginForm
  { username :: Text
  , password :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)

-- | Hashes form (for pause/resume)
newtype HashesForm = HashesForm {hashes :: Text}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)

-- | Delete torrents form
data DeleteTorrentsForm = DeleteTorrentsForm
  { hashes :: Text
  , deleteFiles :: Text -- "true" or "false"
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)

-- | Tags form
data TagsForm = TagsForm
  { hashes :: Text
  , tags :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)

-- | Rename file form
data RenameFileForm = RenameFileForm
  { hash :: Text
  , oldPath :: Text
  , newPath :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)

-- | Rename folder form
data RenameFolderForm = RenameFolderForm
  { hash :: Text
  , oldPath :: Text
  , newPath :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)

-- | Set location form
data SetLocationForm = SetLocationForm
  { hashes :: Text
  , location :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)

-- | Preferences form (json field contains JSON string)
newtype PreferencesForm = PreferencesForm {json :: Text}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)
