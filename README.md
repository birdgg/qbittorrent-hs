# qbittorrent-hs

Haskell client library for the qBittorrent Web API.

**Requires qBittorrent 5.0 or later.** This library does not support qBittorrent 4.x.

## Installation

```cabal
build-depends: qbittorrent
```

## Quick Start

```haskell
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.QBittorrent

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  let config = QBConfig
        { host = "localhost"
        , port = 8080
        , username = "admin"
        , password = "adminadmin"
        , useTLS = False
        }
  env <- mkClientEnv manager config

  -- Login
  result <- runClientM (login config) env
  case result of
    Right "Ok." -> putStrLn "Logged in!"
    _ -> putStrLn "Login failed"

  -- Get torrents
  torrents <- runClientM (getTorrents Nothing) env
  print torrents
```

## API Coverage

### Authentication API ✅ (2/2)

| Endpoint | Status |
|----------|--------|
| `login` | ✅ |
| `logout` | ✅ |

### Application API ✅ (7/9)

| Endpoint | Status |
|----------|--------|
| `version` | ✅ |
| `webapiVersion` | ✅ |
| `buildInfo` | ✅ |
| `shutdown` | ✅ |
| `preferences` | ✅ |
| `setPreferences` | ✅ |
| `defaultSavePath` | ✅ |
| `networkInterfaceList` | ❌ |
| `networkInterfaceAddressList` | ❌ |

### Torrents API ✅ (44/47)

| Endpoint | Status |
|----------|--------|
| `info` | ✅ |
| `properties` | ✅ |
| `trackers` | ✅ |
| `webseeds` | ✅ |
| `files` | ✅ |
| `pieceStates` | ✅ |
| `pieceHashes` | ✅ |
| `add` | ✅ |
| `stop` | ✅ |
| `start` | ✅ |
| `delete` | ✅ |
| `recheck` | ✅ |
| `reannounce` | ✅ |
| `increasePrio` | ✅ |
| `decreasePrio` | ✅ |
| `topPrio` | ✅ |
| `bottomPrio` | ✅ |
| `setFilePrio` | ✅ |
| `setDownloadLimit` | ✅ |
| `setUploadLimit` | ✅ |
| `setShareLimits` | ✅ |
| `setSuperSeeding` | ✅ |
| `setForceStart` | ✅ |
| `setAutoManagement` | ✅ |
| `toggleSequentialDownload` | ✅ |
| `toggleFirstLastPiecePrio` | ✅ |
| `setCategory` | ✅ |
| `categories` | ✅ |
| `createCategory` | ✅ |
| `editCategory` | ✅ |
| `removeCategories` | ✅ |
| `tags` | ✅ |
| `addTags` | ✅ |
| `removeTags` | ✅ |
| `createTags` | ✅ |
| `deleteTags` | ✅ |
| `addTrackers` | ✅ |
| `editTracker` | ✅ |
| `removeTrackers` | ✅ |
| `addPeers` | ✅ |
| `rename` | ✅ |
| `renameFile` | ✅ |
| `renameFolder` | ✅ |
| `setLocation` | ✅ |
| `export` | ✅ |
| `count` | ❌ |
| `downloadLimit` (get) | ❌ |
| `uploadLimit` (get) | ❌ |

### Sync API ✅ (2/2)

| Endpoint | Status |
|----------|--------|
| `maindata` | ✅ |
| `torrentPeers` | ✅ |

### Log API ❌ (0/2)

| Endpoint | Status |
|----------|--------|
| `main` | ❌ |
| `peers` | ❌ |

### Transfer API ❌ (0/7)

| Endpoint | Status |
|----------|--------|
| `info` | ❌ |
| `speedLimitsMode` | ❌ |
| `toggleSpeedLimitsMode` | ❌ |
| `downloadLimit` | ❌ |
| `setDownloadLimit` | ❌ |
| `uploadLimit` | ❌ |
| `setUploadLimit` | ❌ |
| `banPeers` | ❌ |

### RSS API ❌

Not implemented.

### Search API ❌

Not implemented.

## License

MIT
