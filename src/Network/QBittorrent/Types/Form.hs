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
    -- * Torrent Management Forms
  , FilePrioForm (..)
  , LimitForm (..)
  , ShareLimitsForm (..)
  , BoolForm (..)
  , CategoryForm (..)
  , CreateCategoryForm (..)
  , CategoriesForm (..)
  , AddTrackersForm (..)
  , EditTrackerForm (..)
  , RemoveTrackersForm (..)
  , AddPeersForm (..)
  , RenameForm (..)
  , TagsOnlyForm (..)
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

-- | Hashes form (for stop/start)
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

-- | Set file priority form
data FilePrioForm = FilePrioForm
  { hash :: Text
  , id :: Text  -- file indices separated by |
  , priority :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)

-- | Set download/upload limit form
data LimitForm = LimitForm
  { hashes :: Text
  , limit :: Int  -- bytes/second, -1 for unlimited
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)

-- | Set share limits form
data ShareLimitsForm = ShareLimitsForm
  { hashes :: Text
  , ratioLimit :: Double        -- -2 = global, -1 = unlimited
  , seedingTimeLimit :: Int     -- -2 = global, -1 = unlimited (minutes)
  , inactiveSeedingTimeLimit :: Int  -- v5.0+: -2 = global, -1 = unlimited
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)

-- | Boolean setting form (for setSuperSeeding, setForceStart, setAutoManagement)
data BoolForm = BoolForm
  { hashes :: Text
  , value :: Text  -- "true" or "false"
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)

-- | Set category form
data CategoryForm = CategoryForm
  { hashes :: Text
  , category :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)

-- | Create/edit category form
data CreateCategoryForm = CreateCategoryForm
  { category :: Text
  , savePath :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)

-- | Remove categories form
newtype CategoriesForm = CategoriesForm
  { categories :: Text  -- newline separated
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)

-- | Add trackers form
data AddTrackersForm = AddTrackersForm
  { hash :: Text
  , urls :: Text  -- newline separated
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)

-- | Edit tracker form
data EditTrackerForm = EditTrackerForm
  { hash :: Text
  , origUrl :: Text
  , newUrl :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)

-- | Remove trackers form
data RemoveTrackersForm = RemoveTrackersForm
  { hash :: Text
  , urls :: Text  -- separated by |
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)

-- | Add peers form
data AddPeersForm = AddPeersForm
  { hashes :: Text
  , peers :: Text  -- separated by |, each peer is host:port
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)

-- | Rename torrent form
data RenameForm = RenameForm
  { hash :: Text
  , name :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)

-- | Tags only form (for createTags/deleteTags)
newtype TagsOnlyForm = TagsOnlyForm
  { tags :: Text  -- comma separated
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)
