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
    -- * Transfer API Forms
  , TransferLimitForm (..)
  , BanPeersForm (..)
  ) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Network.QBittorrent.Types.InfoHash (InfoHash, hashesToText)
import Network.QBittorrent.Types.Tag (Tag, tagsToText)
import Web.FormUrlEncoded (Form (..), ToForm (..))

-- | Login form
data LoginForm = LoginForm
  { username :: Text
  , password :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)

-- | Hashes form (for stop/start)
newtype HashesForm = HashesForm {hashes :: [InfoHash]}
  deriving stock (Show, Eq, Generic)

instance ToForm HashesForm where
  toForm form =
    Form $
      Map.fromList
        [ ("hashes", [hashesToText form.hashes])
        ]

-- | Delete torrents form
data DeleteTorrentsForm = DeleteTorrentsForm
  { hashes :: [InfoHash]
  , deleteFiles :: Bool
  }
  deriving stock (Show, Eq, Generic)

instance ToForm DeleteTorrentsForm where
  toForm form =
    Form $
      Map.fromList
        [ ("hashes", [hashesToText form.hashes])
        , ("deleteFiles", [if form.deleteFiles then "true" else "false"])
        ]

-- | Tags form
data TagsForm = TagsForm
  { hashes :: [InfoHash]
  , tags :: [Tag]
  }
  deriving stock (Show, Eq, Generic)

instance ToForm TagsForm where
  toForm form =
    Form $
      Map.fromList
        [ ("hashes", [hashesToText form.hashes])
        , ("tags", [tagsToText form.tags])
        ]

-- | Rename file form
data RenameFileForm = RenameFileForm
  { hash :: InfoHash
  , oldPath :: Text
  , newPath :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)

-- | Rename folder form
data RenameFolderForm = RenameFolderForm
  { hash :: InfoHash
  , oldPath :: Text
  , newPath :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)

-- | Set location form
data SetLocationForm = SetLocationForm
  { hashes :: [InfoHash]
  , location :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToForm SetLocationForm where
  toForm form =
    Form $
      Map.fromList
        [ ("hashes", [hashesToText form.hashes])
        , ("location", [form.location])
        ]

-- | Preferences form (json field contains JSON string)
newtype PreferencesForm = PreferencesForm {json :: Text}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)

-- | Set file priority form
data FilePrioForm = FilePrioForm
  { hash :: InfoHash
  , id :: Text  -- file indices separated by |
  , priority :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)

-- | Set download/upload limit form
data LimitForm = LimitForm
  { hashes :: [InfoHash]
  , limit :: Int  -- bytes/second, -1 for unlimited
  }
  deriving stock (Show, Eq, Generic)

instance ToForm LimitForm where
  toForm form =
    Form $
      Map.fromList
        [ ("hashes", [hashesToText form.hashes])
        , ("limit", [T.pack $ show form.limit])
        ]

-- | Set share limits form
data ShareLimitsForm = ShareLimitsForm
  { hashes :: [InfoHash]
  , ratioLimit :: Double        -- -2 = global, -1 = unlimited
  , seedingTimeLimit :: Int     -- -2 = global, -1 = unlimited (minutes)
  , inactiveSeedingTimeLimit :: Int  -- v5.0+: -2 = global, -1 = unlimited
  }
  deriving stock (Show, Eq, Generic)

instance ToForm ShareLimitsForm where
  toForm form =
    Form $
      Map.fromList
        [ ("hashes", [hashesToText form.hashes])
        , ("ratioLimit", [T.pack $ show form.ratioLimit])
        , ("seedingTimeLimit", [T.pack $ show form.seedingTimeLimit])
        , ("inactiveSeedingTimeLimit", [T.pack $ show form.inactiveSeedingTimeLimit])
        ]

-- | Boolean setting form (for setSuperSeeding, setForceStart, setAutoManagement)
data BoolForm = BoolForm
  { hashes :: [InfoHash]
  , value :: Text  -- "true" or "false"
  }
  deriving stock (Show, Eq, Generic)

instance ToForm BoolForm where
  toForm form =
    Form $
      Map.fromList
        [ ("hashes", [hashesToText form.hashes])
        , ("value", [form.value])
        ]

-- | Set category form
data CategoryForm = CategoryForm
  { hashes :: [InfoHash]
  , category :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToForm CategoryForm where
  toForm form =
    Form $
      Map.fromList
        [ ("hashes", [hashesToText form.hashes])
        , ("category", [form.category])
        ]

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
  { hash :: InfoHash
  , urls :: Text  -- newline separated
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)

-- | Edit tracker form
data EditTrackerForm = EditTrackerForm
  { hash :: InfoHash
  , origUrl :: Text
  , newUrl :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)

-- | Remove trackers form
data RemoveTrackersForm = RemoveTrackersForm
  { hash :: InfoHash
  , urls :: Text  -- separated by |
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)

-- | Add peers form
data AddPeersForm = AddPeersForm
  { hashes :: [InfoHash]
  , peers :: Text  -- separated by |, each peer is host:port
  }
  deriving stock (Show, Eq, Generic)

instance ToForm AddPeersForm where
  toForm form =
    Form $
      Map.fromList
        [ ("hashes", [hashesToText form.hashes])
        , ("peers", [form.peers])
        ]

-- | Rename torrent form
data RenameForm = RenameForm
  { hash :: InfoHash
  , name :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)

-- | Tags only form (for createTags/deleteTags)
newtype TagsOnlyForm = TagsOnlyForm
  { tags :: [Tag]
  }
  deriving stock (Show, Eq, Generic)

instance ToForm TagsOnlyForm where
  toForm form =
    Form $
      Map.fromList
        [ ("tags", [tagsToText form.tags])
        ]

-- -----------------------------------------------------------------------------
-- Transfer API Forms
-- -----------------------------------------------------------------------------

-- | Global transfer limit form (for setDownloadLimit/setUploadLimit)
newtype TransferLimitForm = TransferLimitForm
  { limit :: Int  -- bytes/second, 0 for unlimited
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)

-- | Ban peers form
newtype BanPeersForm = BanPeersForm
  { peers :: Text  -- newline separated, format: host:port
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)
